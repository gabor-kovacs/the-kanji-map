import { useContext } from 'react';
import { __RouterContext } from 'react-router-dom';

const useRouter = () => {
	return useContext(__RouterContext);
};

export default useRouter;
