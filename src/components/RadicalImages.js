// import { render } from 'react-dom'
import React, { useState, useEffect } from 'react';
import { useSprings, animated, config } from 'react-spring';

export default function RadicalImages({ radicalImageArray }) {
	const [index, setIndex] = useState(0);

	useEffect(() => {
		const interval = setInterval(() => setIndex((state) => (state + 1) % radicalImageArray.length), 3000);
		return () => {
			clearInterval(interval);
		};
	}, []);

	const springs = useSprings(
		radicalImageArray.length,
		radicalImageArray.map((im, idx) => ({ opacity: index === idx ? 1 : 0, config: config.molasses }))
	);

	return (
		<>
			{springs.map((props, id) => (
				<animated.div
					key={id}
					style={{
						...props,
						backgroundImage: `url(${radicalImageArray[id]})`,
						position: 'absolute',
						width: '100px',
						height: '100px',
						backgroundRepeat: 'no-repeat',
						backgroundSize: 'contain',
					}}
				/>
			))}
		</>
	);
}
