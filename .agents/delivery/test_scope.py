import pathlib
import subprocess
import tempfile
import unittest

import checks
import runtime


class RuntimeScope(unittest.TestCase):
    def test_real_staged_changes_select_delivery_only_and_missing_base_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            def git(*args):
                return subprocess.run(['git','-C',str(root),*args],capture_output=True,text=True,check=True).stdout.strip()
            git('init','-q')
            git('config','user.name','Delivery fixture')
            git('config','user.email','fixture@example.invalid')
            path = root/'.agents/delivery'
            path.mkdir(parents=True)
            (path/'runtime.py').write_text('original')
            (root/'product.txt').write_text('original')
            git('add','.')
            git('commit','-qm','Fixture baseline')
            base=git('rev-parse','HEAD')
            (root/'product.txt').write_text('product change')
            git('add','product.txt')
            self.assertEqual(checks.changed_delivery_files(root,base),[])
            (path/'runtime.py').write_text('actual changed runtime')
            git('add','.agents/delivery/runtime.py')
            self.assertEqual(checks.changed_delivery_files(root,base),['.agents/delivery/runtime.py'])
            with self.assertRaises(runtime.DeliveryError):
                checks.changed_delivery_files(root,'refs/remotes/origin/missing')


if __name__=='__main__':
    unittest.main()
