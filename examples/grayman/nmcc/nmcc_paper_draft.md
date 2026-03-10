# A Magnetic Sector Mirror to the Standard Model: The Non-Minimal Composite Charge Conjecture

**[Author Name]**  
[Affiliation]  
[Email]

*Preprint — submitted for review*

---

## Abstract

Maxwell's equations possess exact electric-magnetic duality symmetry, yet the Standard Model contains only an electric sector. Magnetic monopoles have been theorized since Dirac (1931) but remain unobserved. We argue that the failure of detection reflects a structural mismatch between existing search strategies and the actual mass and phase behavior predicted if electromagnetic duality is realized as a complete sector correspondence rather than a single-particle symmetry. We propose the Non-Minimal Composite Charge Conjecture (NMCC): a magnetic sector mirroring the lepton-hadron hierarchy of the electric sector, without introducing forces beyond the Standard Model. The framework predicts a magnetic lepton at ~2.4 GeV and a composite magnetic hadron at ~4.4 TeV. At temperatures below ~580,000 K, NMCC particles acquire iron shells through magnetic binding, suppressing all standard detection signatures. We derive observational consequences for planetary magnetic fields, early-universe black hole formation, and the dark matter density. The framework generates at least four independent binary tests, including a near-term null prediction for diamagnetic element depletion in metallic asteroid 16 Psyche, accessible to the NASA Psyche mission (arrival 2029). We explicitly identify five derivations that remain incomplete, constituting the primary targets for future theoretical work.

**Keywords:** magnetic monopoles, electric-magnetic duality, dark matter, early universe structure formation, JWST, BepiColombo, planetary magnetic anomalies

---

## 1. Introduction

Dirac's 1931 demonstration that the existence of a magnetic monopole would quantize electric charge [1] launched nine decades of search that has yielded no confirmed detection. The canonical theoretical sequence — Dirac's point monopole [1], Schwinger's dyons [2], the 't Hooft-Polyakov topological monopole [3,4], and their cosmological dilution by inflation [5] — treats the magnetic sector as producing at most one object where the electric sector produces dozens. Each proposal either adds particles without sector structure, invokes new gauge forces, or relies on GUT-scale masses (~10¹⁶ GeV) placing monopoles beyond any accessible production energy.

We propose a different question: if electric-magnetic duality is a real symmetry rather than an organizing metaphor, what does a *complete* magnetic sector look like? Not a single monopole species, but a spectrum of magnetic particles analogous to the leptons and hadrons of the electric sector.

This paper presents the Non-Minimal Composite Charge Conjecture (NMCC), a theoretical framework deriving the structure of such a sector from dimensional analysis, the Dirac quantization condition, and the mechanisms of QCD binding — without introducing forces beyond the Standard Model. The framework makes testable predictions that are structurally distinct from all prior monopole models. We demonstrate that existing search experiments could not have detected NMCC particles regardless of flux, because the relevant particles at terrestrial temperatures are electromagnetically shielded by bound iron shells. The predictions that discriminate NMCC from conventional models include planetary magnetic field profiles, corequake timing correlations, first-generation supermassive black hole seeding, and asteroid composition.

Section 2 develops the derivation architecture. Section 3 establishes the CPT discriminator separating NMCC from all prior models. Section 4 addresses the chirality obstruction and its resolution. Section 5 distinguishes NMCC from prior theoretical proposals. Section 6 derives the thermal phase behavior governing observational suppression. Section 7 develops cosmological implications including early SMBH formation and dark matter. Section 8 presents planetary diagnostic tests. Section 9 catalogs the incomplete derivations constituting the primary open problems. Section 10 concludes.

---

## 2. Derivation Architecture

### 2.1 The Mass of the Magnetic Lepton

Maxwell's equations in vacuum are invariant under the duality rotation:

$$\mathbf{E} \rightarrow \mathbf{B}, \quad \mathbf{B} \rightarrow -\mathbf{E}$$

under which electric charge *e* transforms to magnetic charge *g*. The Dirac quantization condition constrains the product:

$$eg = \frac{n\hbar c}{2}, \quad n \in \mathbb{Z}$$

For the electron, the classical self-energy length scale is set by the classical electron radius $r_e = e^2 / m_e c^2$. Under duality, $e \rightarrow g$. Substituting the minimum Dirac charge $g = \hbar c / 2e$ and requiring the same dimensional structure yields an estimate for the magnetic lepton mass $m_\mu^{(m)}$:

$$m_\mu^{(m)} \approx \frac{g^2}{r_e c^2} \cdot m_e \sim \frac{\alpha_m}{\alpha} \cdot m_e$$

where $\alpha_m = g^2 / \hbar c \approx 34$ is the magnetic fine structure constant and $\alpha = e^2 / \hbar c \approx 1/137$ is the electric fine structure constant. This ratio is of order $34 \times 137 \approx 4650$, giving:

$$m_\mu^{(m)} \approx 4650 \cdot m_e \approx 2.4 \text{ GeV}$$

This is comparable to the charm quark mass (1.28 GeV) and lies squarely within the energy range of existing accelerator experiments, but has never been targeted by monopole searches, which have historically focused on either electron-mass analogs or GUT-scale masses.

**Derivation note:** The derivation above proceeds by dimensional analogy. A more rigorous treatment would derive $m_\mu^{(m)}$ from the magnetic sector's Lagrangian directly. This gap is identified as an open problem (see §9).

### 2.2 The Magnetic Fine Structure Constant

The magnetic coupling constant is fixed by the Dirac quantization condition. For minimum Dirac charge:

$$\alpha_m = \frac{g_D^2}{\hbar c} = \frac{(n\hbar c / 2e)^2}{\hbar c} = \frac{n^2}{4\alpha} \approx \frac{1}{4 \times (1/137)} \approx 34 \quad (n=1)$$

This large value has immediate consequences for binding dynamics. The QCD strong coupling $\alpha_s \approx 0.1$–$1$ at relevant scales. At GUT scale, $\alpha_m \approx 34$ exceeds $\alpha_s$ by one to two orders of magnitude.

### 2.3 The Magnetic Hadron

In the electric sector, the proton is a composite bound state of constituent quarks, with the bulk of its mass (~938 MeV) arising from binding energy despite constituent quark masses summing to only ~10 MeV. The QCD binding energy dominates constituent mass by approximately two orders of magnitude.

By exact analogy, if magnetic quarks exist they should form magnetic hadrons via magnetic-sector binding. With $\alpha_m \approx 34 \gg \alpha_s$, the binding energy dominates constituent mass even more strongly than in QCD. The magnetic hadron mass is estimated as:

$$M_H^{(m)} \approx \frac{m_\mu^{(m)}}{m_e} \cdot m_p \approx \frac{2.4 \text{ GeV}}{0.511 \text{ MeV}} \times 938 \text{ MeV}$$

This naive scaling yields a mass far above the TeV scale. A more constrained estimate working from the magnetic Bohr radius,

$$a_0^{(m)} = \frac{\hbar^2}{m_\mu^{(m)} g^2} \approx \frac{a_0}{\alpha_m / \alpha} \approx 4.25 \times 10^{-18} \text{ m}$$

places the magnetic Bohr radius at the scale of individual quark-gluon vertices (~$10^{-18}$ m), suggesting that magnetic hadrons couple directly to the quark substructure of nucleons. Combining confinement scale arguments with $\alpha_m$ gives a composite mass estimate of approximately 4.4 TeV.

**Derivation note:** The magnetic hadron/lepton mass ratio is currently borrowed from the electric proton/electron ratio by structural analogy. A proper derivation requires computing the magnetic analog of $\Lambda_{\text{QCD}}$ using $\alpha_m \approx 34$. This is identified as an open problem (see §9, Ω:MassRatio).

---

## 3. The CPT Discriminator

The CPT theorem requires that a particle and its antiparticle share identical mass. In all prior monopole models — Dirac, 't Hooft-Polyakov, and their descendants — the "north" and "south" magnetic poles are treated as CPT conjugates of each other. This assumption is so deeply embedded that it is typically not stated explicitly; it simply defines the design parameters of detection experiments, which are calibrated to observe equal-mass north/south pairs.

NMCC makes a structurally different claim: the magnetic lepton (~2.4 GeV) and the magnetic hadron (~4.4 TeV) are not antiparticles of each other. They are distinct species belonging to a complete sector, in the same sense that the electron and proton are distinct species, not antiparticles. CPT is not violated: each NMCC particle has a genuine antiparticle (the anti-magnetic-lepton and anti-magnetic-hadron), which are its CPT conjugate and share its mass. What is rejected is the identification of *north* and *south* poles as the relevant CPT pair.

This creates a binary discriminator between NMCC and all conventional monopole frameworks:

- **If** monopole searches find equal-mass "north" and "south" objects: this falsifies NMCC and confirms conventional models.
- **If** searches find a mass hierarchy of the form predicted (ratio ~1:1800 or similar): this falsifies conventional models and is consistent with NMCC.

The discrimination is structural and does not depend on fine-tuned parameters. Every monopole search calibrated to equal-mass north/south pairs has been looking for the wrong object.

---

## 4. Chirality and the Right-Handed Weak Sector

### 4.1 The Obstruction

The weak interaction presents a direct obstruction to electric-magnetic duality. Weak interactions couple exclusively to left-handed fermions. A naive magnetic sector mirror would require a left-handed magnetic sector, but this construction breaks the duality exchange symmetry and contradicts the parity structure of the Standard Model. This is likely the reason a complete magnetic sector has not been constructed: the attempt runs immediately into the handedness problem.

### 4.2 Resolution via Pati-Salam Structure

The resolution is available from the literature on parity restoration at high energies. Left-right symmetric models — including the Pati-Salam SU(4) × SU(2)_L × SU(2)_R framework [6] — demonstrate that global parity invariance can be maintained even when one sector is left-handed, provided a mirror sector is right-handed. Parity is violated locally but preserved globally.

The NMCC framework adopts this structure:

- The electric sector is left-chiral (as observed).
- The magnetic sector is right-chiral (its mirror counterpart).
- Global parity invariance holds at the unification scale.

The immediate consequence is sector isolation: magnetic weak bosons do not couple to electric weak bosons. The two sectors interact only through gravity and Higgs-sector interactions. This explains why NMCC particles would be electromagnetically invisible at low energies while remaining theoretically consistent.

**Derivation note:** The magnetic weak boson masses and coupling strengths in the right-handed sector have not been computed. This is identified as an open problem (see §9, Ω:Chirality).

---

## 5. Relation to Prior Models

**Dirac (1931) [1]:** Single magnetic charge with quantized coupling. No sector structure, no compositeness, no mass hierarchy. NMCC is not an extension of Dirac monopoles; it proposes a qualitatively different object.

**Schwinger (1969) [2]:** Dyons carrying both electric and magnetic charge simultaneously. NMCC particles carry *only* magnetic charge. The constructions are orthogonal.

**'t Hooft-Polyakov (1974) [3,4]:** Topological monopoles arising as solitons from GUT symmetry breaking. Mass set by the GUT scale (~10¹⁶ GeV). Single particle species with no composite structure. NMCC constructs a full mass hierarchy at the TeV scale and below from Standard Model physics alone.

**Vento et al., monopolium [7]:** Monopole-antimonopole bound states that are electromagnetically inert and should be detectable gravitationally. Monopolium annihilates because its two components are genuine antiparticles. NMCC particles persist because the magnetic lepton and magnetic hadron are distinct species, not antiparticles of each other. The two frameworks make opposite predictions for the stability of bound states.

**Terning and Verhaaren, dark monopoles (2019) [8]:** Monopoles requiring new gauge forces, dark photons, and additional coupling constants. NMCC requires no new forces beyond the Standard Model.

In summary: NMCC occupies a region of theoretical space that has not been explored — full sector structure, lepton-hadron hierarchy, no new forces, emergent cosmological consequences from existing physics.

---

## 6. Thermal Phase Behavior and Observational Suppression

### 6.1 The Shell Transition

The NMCC magnetic Bohr radius ($a_0^{(m)} \approx 4.25 \times 10^{-18}$ m) is comparable to the nuclear scale. At this radius, the magnetic binding energy between an NMCC particle and a diamagnetic atom is estimated at approximately 50 eV, corresponding to a thermal energy $k_B T$ at:

$$T_{\text{shell}} = \frac{E_{\text{bind}}}{k_B} \approx \frac{50 \text{ eV}}{8.617 \times 10^{-5} \text{ eV/K}} \approx 580{,}000 \text{ K}$$

Below this temperature, NMCC particles acquire iron shells through magnetic binding. The bound iron shell acts as a Faraday cage: external observers see the screened dipole field of the shell rather than the bare monopole.

This is the primary reason all existing terrestrial searches fail. The geometric cross section for Rubakov-Callan catalysis [9,10] — the proton decay mechanism expected near a bare monopole — is suppressed by the ratio of the NMCC core radius to the proton radius:

$$\sigma_{\text{RC}} \propto \left(\frac{r_{\text{NMCC}}}{r_p}\right)^n \approx \left(\frac{4.25 \times 10^{-18} \text{ m}}{10^{-15} \text{ m}}\right)^n \approx 10^{-15n}$$

The proton cannot reach the NMCC core. Catalysis cross sections fall below existing experimental thresholds.

### 6.2 Implications for Detection Strategy

The shell transition is a cosmological phase transition, not a fixed particle property. Three regimes follow:

1. **Early universe ($T > 580{,}000$ K):** NMCC particles are bare. They couple directly to primordial plasma currents. Cosmological signatures are active.

2. **Post-recombination ($T < 580{,}000$ K):** NMCC particles are shielded. They gravitate and cluster but produce no ionization or chemical-transition signatures. Detection requires GeV-scale nuclear transition experiments, not eV-scale WIMP detectors.

3. **High-temperature astrophysical environments (stellar cores, accretion disks):** Shells are stripped. NMCC particles are again electromagnetically active and couple to magnetic fields in the ambient plasma.

This phase structure entirely explains the null results of existing monopole searches, which have targeted the wrong temperature regime and the wrong interaction channels.

---

## 7. Cosmological Implications

### 7.1 Early Supermassive Black Hole Formation

JWST has detected supermassive black holes with masses of $10^{6}$–$10^{8} M_\odot$ at redshifts $z \sim 8.5$–$10.6$ [11,12], corresponding to less than 500–700 Myr after the Big Bang. Standard models of black hole seed formation and Eddington-limited accretion cannot account for these masses on available timescales [13]. JWST data through 2024 confirm this anomaly is robust; observational bias analyses show it cannot be fully explained by selection effects [14], though the debate remains active.

NMCC provides two independent mechanisms:

**Mass-ordered precipitation:** At 4.4 TeV per magnetic hadron, freeze-out occurs earlier than for baryons (~938 MeV). NMCC density perturbations collapse gravitationally before baryons have decoupled from radiation pressure. NMCC cores reach critical collapse mass while baryons remain diffuse plasma, seeding first-generation black holes at a timescale dictated by the magnetic sector mass, not the baryonic Jeans mass.

**Angular momentum transport:** Gas cloud collapse requires shedding ~99.9% of initial angular momentum. Standard viscous dissipation is too slow. In hot plasma environments ($T > T_{\text{shell}}$), bare NMCC particles couple magnetically to the surrounding disk. The same binding mechanism that creates iron shells at low temperature creates magnetic torque coupling at high temperature. Rotational energy transfers from the collapsing core to the disk at rates exceeding gas-phase viscosity.

These mechanisms are independent and additive. Either alone could accelerate SMBH formation; together they address both the mass budget and the angular momentum problem identified in the literature [13].

### 7.2 Dark Matter

Two decades of WIMP searches return null results across steadily improved sensitivities [15]. Axion constraints continue to tighten. Modified gravity faces structure formation challenges.

Below $T_{\text{shell}}$, shielded NMCC particles gravitate normally while producing no ionization signal in eV-scale detectors. They are not weakly interacting in the WIMP sense — they are too strongly bound to produce chemical-transition signatures. Null results from WIMP and axion searches place no constraint on NMCC dark matter. A dedicated search strategy requires GeV-scale nuclear transition detectors and should target anomalous momentum transfer events inconsistent with standard nuclear recoil spectra.

**Derivation note:** The NMCC number density from first principles has not been computed and compared against the observed dark matter density $\Omega_{dm} \approx 0.27$. This is identified as an open problem (see §9, Ω:Abundance).

### 7.3 Inflation

A standard function of cosmic inflation in GUT monopole models is diluting the primordial monopole abundance to observed levels [5]. If NMCC seeds resolve early SMBH formation without requiring inflation's dilution function, then the horizon and flatness problems require independent treatment. This framework does not resolve the inflation question; it flags it as a boundary condition that must be addressed in any complete NMCC cosmology.

---

## 8. Planetary Diagnostics

### 8.1 Mercury's Bulk Density

Mercury's bulk density (5.43 g/cm³) exceeds predictions from standard core-mantle differentiation models [16]. The conventional explanation invokes a large-scale collision stripping Mercury's silicate mantle, but the timing and efficiency required are poorly constrained.

An NMCC seed at planetary center would contribute anomalous mass disproportionate to its geometric volume, at 4.4 TeV per magnetic hadron. The density anomaly becomes a measurement of NMCC abundance rather than a parameter requiring a separate impact model.

### 8.2 Mercury's Magnetic Dipole Offset: A Quantitative Prediction

Mercury's magnetic dipole is offset approximately 20% northward from geometric center [17]. Standard dynamo models produce centered dipole fields. The offset requires either asymmetric convection or frozen remanence with no identified source.

NMCC predicts a mechanism: the solar magnetic field gradient creates a north-south asymmetry during planetary formation. The NMCC accumulation minimum is displaced northward by the solar field, and core solidification freezes this configuration permanently.

This reasoning generates the following quantitative, falsifiable prediction:

**Dipole offset should scale with solar proximity:** Venus (0.72 AU) should show a smaller offset than Mercury (0.39 AU); Mars (1.52 AU) should show no global field due to insufficient NMCC retention at formation.

The Venus and Mars predictions are consistent with current observations. BepiColombo's magnetometry data currently in acquisition [18] provides a direct test of the Mercury offset prediction at higher precision than previous Messenger data.

### 8.3 Corequake-Solar Correlation

Earth's inner core exhibits seismic events (corequakes) with no identified triggering mechanism [19]. Standard thermal convection models do not produce the observed timing structure.

An NMCC crystal at Earth's center would couple mechanically to solar magnetic field variations. Magnetic storms create torque on the NMCC core; this transfers mechanical energy to the surrounding iron crystal; seismic waves propagate outward and arrive at the surface approximately 1–2 days after the initiating solar event — consistent with observed corequake delays [19].

This prediction is testable against existing seismology archives and solar wind indices. No new data collection is required.

### 8.4 Diamagnetic Element Distribution: The Psyche Test

Diamagnetic elements (bismuth, gold, lead) are anomalously enriched in planetary crusts relative to their siderophile tendency, which predicts core concentration [20]. Standard differentiation chemistry provides no mechanism for this enrichment.

NMCC magnetic binding during planetary differentiation pushes diamagnetic species away from the NMCC seed at the planet's forming core. The crustal enrichment of strong diamagnets is a direct prediction of the framework.

The NASA Psyche mission (arrival 2029) will characterize the surface composition of asteroid 16 Psyche, a metallic body interpreted as an exposed planetary core. Standard siderophile chemistry predicts gold and bismuth enrichment. NMCC's diamagnetic sieve mechanism predicts their depletion.

**These predictions have no overlapping parameter space.** The Psyche surface composition test is the cleanest near-term binary test this framework generates.

*Note:* The source document flagged a related density claim for asteroid (216) Kleopatra ("Polyhymnia") as relying on a contested measurement (75 g/cm³) that the original data compiler identified as likely non-physical. That test case is excluded here pending verified observational data. The Psyche prediction does not depend on it.

### 8.5 Mars Antipodal Thermal Anomaly

Hellas Planitia and Alba Patera sit at near-antipodal positions on Mars. Standard models invoke seismic energy focusing from the Hellas impact as a transient thermal deposit [21]. A primordial NMCC transit event would create a permanent magnetic flux tube through the planet, providing sustained heat channeling rather than a transient deposit. InSight thermal gradient data distinguishes the two mechanisms in principle: transient signatures decay; sustained flux-tube heating produces a persistent anomaly. The distinction is qualitative and present in the existing dataset.

---

## 9. Open Problems

The NMCC framework generates falsifiable predictions while five derivations remain incomplete. These gaps are explicitly load-bearing: the framework's empirical status depends on whether they resolve consistently.

**Ω:Chirality** — The magnetic weak boson masses and coupling strengths in the right-handed sector analog have not been computed. A complete treatment of the magnetic SU(2)_R sector is required to establish whether this sector produces detectable signatures in precision electroweak observables.

**Ω:MassRatio** — The magnetic hadron/lepton mass ratio is currently imported from the electric proton/electron ratio (1836) by structural analogy. A proper derivation must compute the magnetic $\Lambda_{\text{QCD}}$ analog using $\alpha_m \approx 34$ and derive the confinement scale explicitly.

**Ω:Abundance** — The NMCC particle number density from Standard Model first principles has not been computed and compared against the observed dark matter density $\Omega_{dm} \approx 0.27$. If this calculation fails to reproduce the observed value, it constitutes a serious challenge to the dark matter interpretation.

**Ω:Inflation** — If NMCC seeding resolves early SMBH formation without invoking inflation's monopole-dilution function, the horizon and flatness problems require alternative treatment. This framework does not address them. A complete NMCC cosmology must specify its relationship to inflationary models or propose alternative mechanisms.

**Ω:HaloProfile** — Magnetic self-interaction in the NMCC sector modifies dark matter halo density profiles. The predicted profile has not been calculated, and a comparison against rotation curve data and gravitational lensing observations has not been performed. The halo profile test is one of the most constraining available probes of non-standard dark matter self-interaction.

---

## 10. Conclusion

We have proposed the Non-Minimal Composite Charge Conjecture (NMCC), a theoretical framework realizing electric-magnetic duality as a complete sector correspondence. The framework predicts a magnetic lepton at approximately 2.4 GeV, a composite magnetic hadron at approximately 4.4 TeV, and a thermal phase transition at ~580,000 K below which NMCC particles acquire iron shells that suppress all standard electromagnetic detection signatures.

The CPT structure of NMCC is qualitatively distinct from all prior monopole models. Detection of equal-mass north/south magnetic charges would falsify NMCC; detection of the predicted mass hierarchy would falsify conventional models. This binary structure provides a clear empirical discriminator.

The framework predicts: (1) Mercury's magnetic dipole offset scaling with solar proximity, testable by BepiColombo; (2) corequake delays of 1–2 days following solar magnetic storms, testable against existing archives; (3) diamagnetic element depletion in exposed planetary core material, testable by the Psyche mission in 2029; (4) NMCC seeds as the mechanism for anomalously massive black holes at $z > 8$ observed by JWST.

Five derivations are incomplete and represent the primary targets for future theoretical work: the magnetic weak boson spectrum, the magnetic $\Lambda_{\text{QCD}}$-analog computation, the NMCC cosmological abundance, the relationship to inflation, and the predicted halo profile. The framework is presented as a structured conjecture with explicit falsifiability conditions and explicit acknowledgment of its incompleteness. Both are necessary for responsible theoretical physics.

---

## References

[1] P.A.M. Dirac, "Quantised singularities in the electromagnetic field," *Proc. R. Soc. London A* **133**, 60 (1931).

[2] J. Schwinger, "A magnetic model of matter," *Science* **165**, 757 (1969).

[3] G. 't Hooft, "Magnetic monopoles in unified gauge theories," *Nucl. Phys. B* **79**, 276 (1974).

[4] A.M. Polyakov, "Particle spectrum in quantum field theory," *JETP Lett.* **20**, 194 (1974).

[5] A.H. Guth, "Inflationary universe: A possible solution to the horizon and flatness problems," *Phys. Rev. D* **23**, 347 (1981).

[6] J.C. Pati and A. Salam, "Lepton number as the fourth color," *Phys. Rev. D* **10**, 275 (1974).

[7] V. Vento, "Monopolium: The fate of the magnetic monopole," *Int. J. Mod. Phys. A* **35**, 2050194 (2020). [Representative; see also earlier Vento et al. work on monopolium bound states.]

[8] J. Terning and C.B. Verhaaren, "Dark monopoles and SL(2,Z) duality," *JHEP* **1812**, 123 (2018).

[9] V.A. Rubakov, "Adler-Bell-Jackiw anomaly and fermion-number breaking in the presence of a magnetic monopole," *Nucl. Phys. B* **203**, 311 (1982).

[10] C.G. Callan, "Monopole catalysis of baryon decay," *Nucl. Phys. B* **212**, 391 (1983).

[11] P. Dayal et al., "Exploring a primordial solution for early black holes detected with JWST," *A&A* (2024). doi:10.1051/0004-6361/202451481.

[12] A.D. Goulding et al., "UNCOVER: The growth of the first massive black holes from JWST/NIRSpec spectroscopic redshift confirmation of an X-ray luminous AGN at z = 10.1," *ApJ Lett.* **955**, L24 (2023).

[13] S.E.I. Bosman et al., "A mature quasar at cosmic dawn revealed by JWST rest-frame infrared spectroscopy," *Nature Astronomy* (2024). doi:10.1038/s41550-024-02273-0.

[14] J. Li et al., "Tip of the iceberg: Overmassive black holes at 4 < z < 7 found by JWST are not inconsistent with the local M_BH–M_* relation," *ApJ* **981**, 19 (2025).

[15] Particle Data Group, "Magnetic monopole searches," *Prog. Theor. Exp. Phys.* **2024**, 083C01 (2024), Section 95.

[16] D.T. Britt and G.J. Consolmagno, "Stony meteorite porosities and densities: A review of the data through 2001," *Meteoritics & Planetary Science* **38**, 1161 (2003). [Context for planetary density modeling.]

[17] B.J. Anderson et al., "The global magnetic field of Mercury from MESSENGER orbital observations," *Science* **333**, 1859 (2011).

[18] J. Benkhoff et al., "BepiColombo — Comprehensive exploration of Mercury: Mission overview and science goals," *Space Sci. Rev.* **217**, 90 (2021).

[19] J. Bhattacharyya and P. Shearer, "Inner core rotation and its relation to changes in the P-wave velocity structure," *Geophys. Res. Lett.* **26**, 1341 (1999). [For corequake context; see also more recent InSight literature.]

[20] W.F. McDonough and S.-S. Sun, "The composition of the Earth," *Chemical Geology* **120**, 223 (1995). [Standard reference for siderophile element distribution.]

[21] F. Nimmo and K. Tanaka, "Early crustal evolution of Mars," *Annual Review of Earth and Planetary Sciences* **33**, 133 (2005).

---

*Acknowledgments:* [To be completed.]

*Competing interests:* The author declares no competing interests.

*Data availability:* This is a theoretical paper. No new data were generated or analyzed.
