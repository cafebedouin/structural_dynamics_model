% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot Wave Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The pilot wave reading of quantum mechanics proposes that particles have
 *   definite positions guided by a real physical wavefunction field, that
 *   determinism is restored through hidden variables, and that the apparent
 *   indeterminism of Copenhagen is an artifact of epistemic limitation, not
 *   ontological randomness. This reading is instantiated ONE WAY to interpret
 *   the contested kernel of quantum formalism — others (Copenhagen,
 *   many-worlds) are separate constraints. The pilot wave reading operates as
 *   both genuine coordination (it solves the pedagogical problem of talking
 *   about quantum mechanics in classical ontological terms) and extraction
 *   (it enforces adherence to its ontological commitments as the price of
 *   legitimate participation in foundational quantum theory debate). The
 *   constraint's persistence depends on active enforcement: pilot wave
 *   theorists maintain publication venues, research programs, and citation
 *   networks that privilege pilot wave work, suppress competing framings
 *   within foundational theory, and extract legitimacy from Copenhagen by
 *   positioning it as the 'old view' whose problems pilot wave solves.
 *
 * KEY AGENTS:
 *   - Pilot wave theorists: maintain the research program, control the canonical formalism variants, enforce entry standards
 *   - Determinism-preservation constituency: benefits from the reading's restoration of determinism, non-controlling but rhetorically aligned
 *   - Copenhagen adherents: institutionally dominant but displaced by pilot wave's counter-narrative, constrained exit (embedded in pedagogy/practice)
 *   - Alternative heterodox interpretations (many-worlds, objective collapse, QBism): forced to compete for recognition against pilot wave's 'classical restoration' framing
 *   - Experimental physicists: payers (forced to adopt ontological commitment) and beneficiaries (classical ontology aligns with intuition), mobile exit
 *   - Mathematics and logic community: observers, conduct formal analysis of logical structure
 *   - Quantum gravity researchers: excluded, have stakes in which interpretation constrains quantum gravity but not yet in the conversation
 *   - Pedagogical institutions: observe pressure from pilot wave reading on how quantum mechanics is taught
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.62).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '77bfef06-a820-4da0-ad54-7a7c389906bd').
narrative_ontology:cs_kernel_codification('77bfef06-a820-4da0-ad54-7a7c389906bd', fixed_text).
narrative_ontology:cs_authority_grounding('77bfef06-a820-4da0-ad54-7a7c389906bd', lineage).
narrative_ontology:cs_interpretation_layer_present('77bfef06-a820-4da0-ad54-7a7c389906bd').
narrative_ontology:cs_reading_relation('77bfef06-a820-4da0-ad54-7a7c389906bd', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('77bfef06-a820-4da0-ad54-7a7c389906bd', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('77bfef06-a820-4da0-ad54-7a7c389906bd', foundational, particles_have_definite_positions).
narrative_ontology:cs_axiom_status(particles_have_definite_positions, holdable).
narrative_ontology:cs_axiom_grounding('77bfef06-a820-4da0-ad54-7a7c389906bd', particles_have_definite_positions, deontological).
narrative_ontology:cs_axiom('77bfef06-a820-4da0-ad54-7a7c389906bd', foundational, determinism_preserved_via_hidden_variables).
narrative_ontology:cs_axiom_status(determinism_preserved_via_hidden_variables, holdable).
narrative_ontology:cs_axiom_grounding('77bfef06-a820-4da0-ad54-7a7c389906bd', determinism_preserved_via_hidden_variables, deontological).
narrative_ontology:cs_reference_frame('77bfef06-a820-4da0-ad54-7a7c389906bd', classical_particle_ontology_framework).
narrative_ontology:cs_drift_state('77bfef06-a820-4da0-ad54-7a7c389906bd', contemporary_quantum_mechanics, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77bfef06-a820-4da0-ad54-7a7c389906bd', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, pilot_wave_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, determinism_preservation_constituency).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, copenhagen_adherents).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, measurement_problem_heterodoxy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, experimental_physicists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, experimental_physicists).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, classical_ontology_restoration).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, deterministic_hidden_variable_locality).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, wavefunction_as_physical_field).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A research program centered on de Broglie-Bohm interpretations and Bohmian mechanics. They set the canonical reading of quantum mechanics as one where particles have definite positions guided by a real physical wavefunction field (pilot wave), and hidden variables restore determinism and locality at the particle level. They control publication venues (journals specializing in interpretations, conferences on foundational physics), define which variants count as 'legitimate' pilot wave work, and manage citation patterns that privilege pilot wave research. Their enforcement operates through peer review, hiring decisions in foundational quantum theory, and the authority to adjudicate what counts as a 'serious' alternative to Copenhagen.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pilot_wave_theorists, agenda_setter,
    organized, generational, arbitrage, global).

% Philosophers and physicists who value determinism as an epistemic or ontological commitment and who see pilot wave as vindicating determinism in quantum mechanics. They benefit rhetorically from the constraint's operation: it positions determinism as something preserved or recovered through quantum mechanics, not abandoned. They do not control the research program but are aligned with its core narrative. Many work in philosophy of physics, foundations of quantum mechanics, or quantum gravity (where determinism may be a prerequisite for a coherent theory). Their stakes are primarily philosophical: determinism is positioned as compatible with modern physics rather than refuted by it.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, determinism_preservation_constituency, beneficiary,
    moderate, generational, arbitrage, global).

% The institutional dominant reading of quantum mechanics, which treats the wavefunction as an epistemic tool (not a real field), wavefunction collapse as marking an absolute boundary between quantum and classical, and indeterminism as fundamental to quantum mechanics. They are 'payers' because the pilot wave reading suppresses Copenhagen's monopoly on 'what quantum mechanics really means.' Their exit options are heavily constrained: Copenhagen is embedded in standard textbooks, the default framework taught to physicists worldwide, the implicit reasoning in most experimental and applied quantum mechanics, and the framework of most working quantum physicists who do not specialize in interpretations. Abandoning Copenhagen means rewriting curricula, retraining students, and challenging the default intuitions of the entire field.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, copenhagen_adherents, payer,
    powerful, biographical, constrained, global).

% Alternative interpretations of quantum mechanics: many-worlds (decoherence, no collapse), objective collapse theories (GRW, CSL), relational interpretations, QBism, statistical interpretations, objective approaches. They bear the cost of the pilot wave reading's operation because it frames itself as 'the classical restoration' — the natural, historically continuous reading — positioning other heterodox approaches as either 'strange' (many-worlds, QBism) or 'not seriously considered' (relational, objective collapse). Their exit options are constrained: building competing research programs requires securing funding, publication venues, academic positions, and citation networks that can challenge the established interpretation communities (Copenhagen dominant, pilot wave growing). Each heterodox approach must compete separately; no unified counter-narrative exists.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, measurement_problem_heterodoxy, payer,
    moderate, biographical, constrained, global).

% Laboratory physicists, quantum information researchers, applied quantum engineers. They use quantum mechanics instrumentally and are largely indifferent to interpretation debates. However, the pilot wave reading imposes a cost: if they want to justify their work philosophically (in grants, papers, or departmental context), they are pressured to adopt a specific ontological commitment — particles have definite positions, determinism holds, etc. — to avoid being classified as 'mere instrumentalists.' However, they also benefit: classical ontology and determinism align with their intuitive reasoning about how physics 'works,' so adopting pilot wave feels natural. Their exit options are mobile: they can use the formalism instrumentally without commitment, adopt whichever interpretation best fits their research context, or ignore the interpretation question entirely.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, experimental_physicists, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, experimental_physicists, beneficiary).

% Formal logicians, mathematicians, and proof theorists who examine the logical structure of quantum formalisms and all interpretations. They conduct formal analysis to show which interpretations are logically consistent, which face formal obstacles, which are empirically equivalent, and which make distinct predictions. They have no direct stake in which interpretation wins but produce the formal results that constrain legitimate claims. Their role is observation and formal verification, not enforcement or benefit.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mathematics_and_logic_community, observer,
    analytical, civilizational, analytical, global).

% Researchers working on reconciling quantum mechanics with general relativity (string theory, loop quantum gravity, asymptotic safety, causal set theory, etc.). They have a direct stake in which interpretation becomes canonical: a deterministic, particle-based interpretation (pilot wave) may impose different constraints on quantum gravity than an indeterministic, wavefunction-collapse-based interpretation (Copenhagen) or a many-worlds interpretation. However, they are currently excluded from interpretation debates — foundational interpretation discussions have not yet been forced to confront quantum gravity constraints. Their exclusion is structural: the interpretation question precedes the quantum gravity program, and gravity researchers are not yet in the room where interpretation commitments are made.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_gravity_researchers, excluded,
    organized, civilizational, constrained, global).

% Universities, physics departments, textbook publishers, accreditation bodies, national science curricula. They observe which interpretation is dominant in research literature, which is easiest to teach clearly, which is mandated by accreditation or institutional tradition. Currently, Copenhagen dominates undergraduate and graduate pedagogy (Griffiths, Sakurai, Shankar textbooks teach Copenhagen as default). The pilot wave reading exerts institutional pressure: as pilot wave research grows and Bohmian mechanics is taught in more departments, pedagogy may shift to include pilot wave as an alternative or third perspective. Pedagogical institutions have constrained exit: they cannot easily rewrite curricula or textbooks without institutional commitment and collective action.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pedagogical_institutions, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, pilot_wave_theorists).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, particle-based ontological reading of quantum mechanics that speaks in classical language: particles have definite positions, determinism holds, the wavefunction is a real physical field guiding particle motion, measurement reveals pre-existing values rather than creating them, the observer is eliminable. Solves the coordination problem of how to interpret and discuss quantum mechanics in terms that map onto classical physics intuitions and preserve determinism as an intuitive feature of reality.
% TRANSFER_FUNCTION: Transfers epistemological authority from Copenhagen-based indeterministic interpretation to a deterministic, particle-based, realist interpretation. The reading moves legitimacy: to be a serious participant in foundational quantum theory discussions, one must reckon with pilot wave as a valid, coherent alternative to Copenhagen, not a historical footnote. This elevation of pilot wave's authority comes at the cost of Copenhagen's monopoly on 'what quantum mechanics really means.' The transfer is not merely rhetorical — it is enforced through publication bias, hiring decisions, citation patterns, and curriculum pressure.
% ABSENT_VOICES: Quantum gravity researchers, who should argue that the interpretation question is upstream to quantum gravity and that different interpretations impose different constraints on quantum-gravity programs (e.g., determinism might be necessary for causal structure in quantum gravity; wavefunction realism might impose different boundary conditions). These researchers are excluded from interpretation debates because interpretation discussions predate quantum gravity's need to choose. Instrumental pragmatists — those who believe no interpretation should be canonical and that physicists should remain agnostic about 'what QM really means' — are structurally marginalized by the framing of 'which reading is correct.' Philosophers working outside physics departments or non-anglophone philosophy of physics communities are excluded from peer-review and publication infrastructure that determines which readings are canonical.
% DISAPPEARANCE_RATIONALE: If the pilot wave reading vanished — if Bohm's 1952 work had never been published, if de Broglie-Bohm interpretations were unknown, if the determinism-preservation narrative were unavailable — quantum mechanics would continue to work identically, but the foundational interpretation debate would be restructured. Copenhagen would remain the institutional dominant reading without a serious competing 'classical restoration' alternative. Pedagogy would not include the 'third way' of recovering determinism and particle ontology. Alternative heterodox interpretations (many-worlds, objective collapse) would not have 'Bohm' as a reference point or model. Philosophy of physics would lack the canonical example of 'how to preserve classical intuitions in a quantum world.' The appearance of foundational debates would rearrange from a three-pole conversation (Copenhagen, many-worlds, pilot-wave) to a two-pole conversation (Copenhagen vs. many-worlds). The institutional practices of quantum mechanics — research programs, publications, citations, hiring — would redirect authority differently.
% FOUNDING_PROBLEM: Early quantum mechanics faced an apparent trilemma: (1) accept wavefunction collapse and irreducible indeterminism (Copenhagen), (2) give up single-world realism and accept many parallel branches (many-worlds), or (3) abandon the classical particle picture entirely and accept that quantum systems lack definite properties until measured. de Broglie (1927) and Bohm (1952) showed a potential exit: quantum mechanics can be reinterpreted with definite particle positions, deterministic evolution, and a real wavefunction field guiding the particles, reconciling quantum mechanics with classical ontology and determinism. The founding problem: 'Can classical ontology and determinism be preserved in quantum mechanics without abandoning empirical adequacy or introducing hidden variables that lead to logical contradictions?'
% FOUNDING_PROBLEM_CORROBORATION: de Broglie (1927) and Bohm (1952) originated the mathematical framework showing classical ontology could be preserved; later theorists (Bell 1966, Dürr-Goldstein, Teufel, Allori) developed rigorous formalism and defenses. Pilot wave advocates and determinism-preservation theorists attest the founding problem is live: Copenhagen's indeterminism and many-worlds' branching remain under challenge, and pilot wave shows these are not inevitable consequences of quantum mechanics. Copenhagen adherents and many-worlds advocates attest the founding problem is no longer live: quantum mechanics works perfectly without restoring classical ontology, determinism adds no predictive or explanatory power, and the choice for pilot wave is philosophical preference decoupled from empirical necessity. Independent mathematical analysis (van Fraassen, Wallace, others) confirms that pilot wave and Copenhagen are empirically equivalent — they generate identical predictions for all known experiments — so the founding problem's resolution cannot be empirical; it is a choice of metaphysics. No outside party (experimental data, new physics, formal theorems) has adjudicated the founding problem; it remains philosophical and subject to disciplinary politics.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the pilot wave reading operates on two fronts: (1) it coordinates legitimate ontological interpretation of quantum mechanics (genuine function — coordinates how to speak about QM in classical terms), and (2) it extracts legitimacy from competing readings by positioning them as 'wrong views' or 'incomplete' (Copenhagen lacks determinism, many-worlds lacks single-world realism). The extraction grows over time (0.35→0.62 from t=0 to t=75, spanning post-1952 to contemporary): as the research program matures, its enforcement infrastructure strengthens, and more graduate students and researchers are socialized into pilot wave commitments. Suppression is high (0.71) because the constraint's persistence depends on actively suppressing alternative framings of quantum mechanics: Copenhagen remains pedagogically dominant, but the pilot wave reading suppresses Copenhagen's legitimacy claim as the 'final word' on quantum meaning. The suppression mechanism is enforcement through publication bias, citation patterns, and identity-boundary policing (what counts as 'serious' work on quantum interpretations). Theater is elevated (0.48) because much of the pilot wave reading's operation is performative maintenance: formal mathematics showing pilot wave is internally consistent (real), philosophical arguments about why determinism is superior (real), but also ritual citation of Bohm's 1952 paper, canonicalization of specific variants (Dürr-Goldstein formalism) over others, and maintenance of the 'lost classic' narrative (pilot wave was suppressed, now rightfully restored). The theater ratio grows over time (0.32→0.48) as the constraint matures: more effort goes into defending pilot wave against heterodox challenges than into proving it superior to Copenhagen (empirically equivalent readings cannot be distinguished empirically, so enforcement becomes rhetorical and institutional rather than evidential).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Copenhagen, heterodoxy) and the agenda-setter seat (pilot wave theorists) should compute radically different types from this same structural data. From the pilot wave seat: genuine coordination solving the classical-ontology problem, benefiting everyone who cares about determinism, an intellectual tradition rightfully recovered. From the Copenhagen payer seat: a competitive narrative suppressing the empirically adequate framework, extracting legitimacy through institutional dominance, enforcing philosophical commitment decoupled from empirical evidence. The engine computes this divergence directly from power + exit + beneficiary/victim + directionality; the authored claim (tangled_rope) and metrics (moderate-high extractiveness, high suppression, moderate theater) should ground the divergence in structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Pilot wave theorists are beneficiaries (they control the reading, set the agenda, maintain the research program) — their d is near 0.0 (subsidy: the constraint's operation distributes legitimacy to them). Determinism-preservation constituency benefits (d near 0.2: they collect philosophical vindication, but do not control the program). Copenhagen adherents are payers (they lose institutional authority, are forced to defend against counter-narratives, have constrained exit — d near 0.8: high extraction). Measurement problem heterodoxy are payers (they must compete for recognition against the pilot wave framing, d near 0.75). Experimental physicists sit near symmetric (d~0.5): they pay the cost of ontological commitment, benefit from classical ontology, have mobile exit. The directionality derivation should weight: beneficiary status (pilots, determinism constituency) toward low d; victim status (Copenhagen, heterodoxy) toward high d; constrained exit (Copenhagen embedded in textbooks and practice) upward; mobile exit (experimental physicists) toward symmetric; organizational power (pilot wave theorists) keeps high-power beneficiaries at low d despite high power (power amplifies extraction for targets, dampers it for beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem's status (contested) + disappearance verdict (world rearranges) flags a potential zombie constraint: if the founding problem (can classical ontology be preserved?) were resolved in Copenhagen's favor, or if the constraint vanished entirely, quantum mechanics would continue to function and be taught, but the conversation about what QM 'really means' would be restructured around different poles. The constraint is not yet mandatropic — it maintains both genuine coordination (ontological legitimacy talk) and extraction (institutional dominance) — but the mismatch between founding problem (contested/not resolved) and persistence (the constraint remains despite empirical equivalence to Copenhagen) signals mandatrophy is incipient. A future state where the constraint persists mainly through institutional inertia while the founding problem is effectively dead (most research treat Copenhagen and pilot wave as equivalent tools, pilot wave no longer pressed as 'the right view') would be mandatrophy. Current state: live founding problem + contested status + active enforcement = tangled rope, not yet piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_under_determination,
    'If pilot wave and Copenhagen make identical empirical predictions (as proven by van Fraassen and others), what justifies treating pilot wave as extracting legitimacy rather than merely offering alternative ontology?',
    'Discover an empirical difference between pilot wave and Copenhagen predictions (e.g., tests of contextuality, violations of Bell inequalities under specific interpretations). If no empirical difference can be found, the constraint is purely philosophical, and its extraction is entirely institutional/rhetorical.',
    'If empirically equivalent, the extraction is confirmed to be institutional rather than evidential — the constraint operates by rhetorical advantage, not by demonstrating pilot wave is correct. This would lower the legitimacy of the reading''s enforcement and potentially reclassify the constraint as more snare-like (pure extraction) than tangled_rope. If a difference emerges, pilot wave could claim empirical superiority, lowering extraction below institutional rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_equivalence_under_determination, empirical, 'Whether pilot wave and Copenhagen are empirically distinguishable or equivalently predictive.').

omega_variable(
    pedagogical_inevitability_of_copenhagen,
    'Is Copenhagen''s dominance in undergraduate and graduate quantum mechanics pedagogy a consequence of its genuine superiority, or of institutional inertia and the simplicity of teaching Copenhagen without discussing alternatives?',
    'Conduct a pedagogical experiment: teach pilot wave mechanics in parallel with Copenhagen at the same institution, using comparable textbooks and instructor competence, and measure student understanding, retention, and conceptual mastery. Compare to a control group taught Copenhagen alone.',
    'If pilot wave students show no measurable disadvantage and possibly better classical intuition, the constraint''s suppression of Copenhagen is less justified by pedagogy and more justified by institutional enforcement — extraction rises. If Copenhagen students retain quantum concepts better, pilot wave''s extraction is partly justified (Copenhagen is genuinely simpler for learning), and the constraint is less extractive than currently measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_inevitability_of_copenhagen, empirical, 'Whether Copenhagen''s pedagogical dominance reflects superior pedagogy or institutional inertia.').

omega_variable(
    kernel_vs_reading_distinction,
    'Is the distinction between the QUANTUM_FORMALISM kernel and each reading (pilot_wave, copenhagen, many_worlds) stable, or does the choice to treat pilot wave as a ''reading'' rather than ''the truth'' itself presuppose a relativistic framing that favors Copenhagen or many-worlds?',
    'A pilot wave theorist might argue that the kernel is not ''quantum formalism'' but ''ontology of quantum mechanics,'' and the pilot wave reading IS what the quantum formalism says — making Copenhagen and many-worlds the aberrant readings. Alternatively, a mathematician might argue the kernel is the mathematical formalism itself (Hilbert spaces, operators, Born rule), and readings are epistemic overlays on stable mathematics. Resolve by asking: what is the stable fact across all readings, and what varies? If what varies is ontology only (math is the same), then readings are authorized. If what varies is which math is ''correct'' to use, then readings may be illusory.',
    'If the distinction is unstable, the entire committer frame is compromised — we cannot say ''here is the kernel, here are readings'' because no kernel exists independent of reading commitments. This would reclassify the constraint from tangled_rope to snare: pure institutional dominance with no genuine coordination function. If the distinction is stable (math is the kernel, ontology is reading overlay), the constraint retains coordination function and remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Whether the kernel/reading distinction for quantum formalism is stable or presupposes one reading''s truth.').

omega_variable(
    nonlocality_as_cost_vs_feature,
    'Pilot wave mechanics requires nonlocal guidance of particles by the wavefunction. Some advocates treat this nonlocality as a feature (respecting relativity''s spirit by maintaining local fields and particle trajectories), others as a cost (violating our intuitions about how influences propagate). Does the constraint extract by hiding this cost in the classical ontology benefit, or is nonlocality genuinely compatible with classical metaphysics?',
    'Formal analysis: prove whether nonlocal guidance is logically compatible with classical particle metaphysics, or whether accepting nonlocality is already a departure from classical ontology that undermines pilot wave''s claim to ''restore'' classicality. Consult Bell, Jarrett, Shimony on separability and locality.',
    'If nonlocality is incompatible with classical metaphysics, pilot wave''s central claim (restore classical ontology) is partially false, and the extraction it achieves is based on a false premise — the constraint would be more snare-like (deceptive framing). If nonlocality is compatible (perhaps as a reinterpretation of ''classical''), the extraction is justified, and the constraint remains tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonlocality_as_cost_vs_feature, conceptual, 'Whether nonlocality is compatible with pilot wave''s claim to restore classical ontology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__pilot_wave_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(quan_tr_t15, observed).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__pilot_wave_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(quan_tr_t30, observed).
narrative_ontology:measurement(quan_tr_t45, quantum_formalism__pilot_wave_reading, theater_ratio, 45, 0.46).
narrative_ontology:measurement_basis(quan_tr_t45, observed).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__pilot_wave_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(quan_tr_t60, observed).
narrative_ontology:measurement(quan_tr_t75, quantum_formalism__pilot_wave_reading, theater_ratio, 75, 0.48).
narrative_ontology:measurement_basis(quan_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__pilot_wave_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(quan_be_t15, observed).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__pilot_wave_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(quan_be_t30, observed).
narrative_ontology:measurement(quan_be_t45, quantum_formalism__pilot_wave_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(quan_be_t45, observed).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__pilot_wave_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(quan_be_t60, observed).
narrative_ontology:measurement(quan_be_t75, quantum_formalism__pilot_wave_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(quan_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__pilot_wave_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(quan_su_t15, observed).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__pilot_wave_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(quan_su_t30, observed).
narrative_ontology:measurement(quan_su_t45, quantum_formalism__pilot_wave_reading, suppression_requirement, 45, 0.69).
narrative_ontology:measurement_basis(quan_su_t45, observed).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__pilot_wave_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(quan_su_t60, observed).
narrative_ontology:measurement(quan_su_t75, quantum_formalism__pilot_wave_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(quan_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.12).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the quantum_formalism kernel. The pilot_wave_reading is structured as a constraint because it operates in the world as an enforced interpretation — a set of claims about what quantum mechanics 'really means' that practitioners are socialized into, that control publication and pedagogy, that extract legitimacy from competing readings. Sibling readings (copenhagen_reading, many_worlds_reading) are separate constraints with their own ε, beneficiary/victim structures, and institutional operations. The three constraints form a constraint family linked by network.affects_constraints. Each has a different claim_type reflecting its core assertion: pilot wave claims determinism+particles+field, Copenhagen claims collapse+indeterminism+observer, many-worlds claims determinism+branching+no-observer. The ε values differ substantially: pilot wave is extractive (it enforces ontological commitment), Copenhagen is also extractive (institutional dominance), many-worlds is extractive (enforces branching ontology). Each reading invokes different beneficiaries (pilot wave: determinism advocates; Copenhagen: indeterminism advocates; many-worlds: realism advocates) and different victims (pilot wave: Copenhagen adherents; Copenhagen: determinism advocates; many-worlds: single-world advocates). The engine will compute per-reading, per-seat classifications; the corpus records the family as three linked stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
