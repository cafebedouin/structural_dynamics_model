% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading: Universal Unitary Dynamics with Decoherent Branching
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The many-worlds reading constrains how quantum theory may be interpreted:
 *   the universal wavefunction evolves unitarily without exception,
 *   measurement is not a physical process but decoherence-induced branching,
 *   and every outcome is realized in some branch. The reading presents itself
 *   as natural law — 'the formalism, taken literally; nothing added' — and
 *   that self-presentation is authored here as the claimed type (mountain,
 *   emerges_naturally). The metrics describe the arrangement's actual
 *   operation: an advocacy-sustained interpretive settlement with
 *   identifiable beneficiaries (the Everettian research program, quantum
 *   cosmology, the decoherence specialty), real if soft extraction (framing
 *   control, marginalization of rivals, a deferred probability problem), and
 *   persistent organized resistance. Whether the naturality claim survives
 *   contact with the auxiliary machinery the arrangement actually requires is
 *   left open as the lead omega; the false-summit signature evaluates exactly
 *   this gap. Interval mapping: t=0..65 covers 1957 (Everett's thesis)
 *   through 2022. CONSTRAINT FAMILY: this story is one reading of the kernel
 *   quantum_formalism. Sibling readings are separate constraints in separate
 *   files: copenhagen_reading (collapse as physical process; indeterminism
 *   primitive) and pilot_wave_reading (definite positions guided by the
 *   wave). Each authors its own epsilon over its own arrangement: this story
 *   authors epsilon=0.22 over the Everettian arrangement as the reading
 *   itself assesses it — low because the reading claims to add nothing to the
 *   formalism, nonzero because it concedes the extravagance burden and the
 *   deferred probability problem. The siblings' epsilon values belong to
 *   their files; cross-reading comparison without per-reading epsilon is
 *   invalid (see omega kernel_reading_commitment_structure). KEY AGENTS (by
 *   structural relationship): - everettian_foundations_researchers: Primary
 *   beneficiary (organized/identity_locked) — collects career capital,
 *   citation networks, and program legitimacy - quantum_cosmology_community:
 *   Secondary beneficiary (organized/constrained) — receives the only
 *   developed observer-free framework - decoherence_researchers:
 *   Dual-positioned beneficiary/payer (organized/mobile) — made load-bearing,
 *   absorbs the objection traffic - rival_interpretation_advocates: Primary
 *   payer (moderate/identity_locked) — bears marginalization and rebuttal
 *   friction - physics_graduate_students: Diffuse payer (powerless/mobile) —
 *   inherits a closed-seeming question set - collapse_model_experimentalists:
 *   Excluded voice (moderate/constrained) — empirically decisive program kept
 *   peripheral - foundations_gatekeepers: Agenda setter
 *   (institutional/arbitrage) — administers circulation of framings -
 *   analytical_philosophers_of_physics: Analytical observer
 *   (analytical/analytical) — audits derivations from outside allegiance
 *
 * KEY AGENTS:
 *   - everettian_foundations_researchers: Primary beneficiary (organized/identity_locked) — collects career capital, citation networks, and program legitimacy
 *   - quantum_cosmology_community: Secondary beneficiary (organized/constrained) — receives the only developed observer-free framework
 *   - decoherence_researchers: Dual-positioned beneficiary/payer (organized/mobile) — made load-bearing, absorbs the objection traffic
 *   - rival_interpretation_advocates: Primary payer (moderate/identity_locked) — bears marginalization and rebuttal friction
 *   - physics_graduate_students: Diffuse payer (powerless/mobile) — inherits a closed-seeming question set
 *   - collapse_model_experimentalists: Excluded voice (moderate/constrained) — empirically decisive program kept peripheral
 *   - foundations_gatekeepers: Agenda setter (institutional/arbitrage) — administers circulation of framings
 *   - analytical_philosophers_of_physics: Analytical observer (analytical/analytical) — audits derivations from outside allegiance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.22).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.34).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading: Universal Unitary Dynamics with Decoherent Branching").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).
domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '3fabacd6-87ed-4a3b-8f18-ff2532d00112').
narrative_ontology:cs_kernel_codification('3fabacd6-87ed-4a3b-8f18-ff2532d00112', formalized).
narrative_ontology:cs_authority_grounding('3fabacd6-87ed-4a3b-8f18-ff2532d00112', expertise).
narrative_ontology:cs_interpretation_layer_present('3fabacd6-87ed-4a3b-8f18-ff2532d00112').
narrative_ontology:cs_reading_relation('3fabacd6-87ed-4a3b-8f18-ff2532d00112', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('3fabacd6-87ed-4a3b-8f18-ff2532d00112', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('3fabacd6-87ed-4a3b-8f18-ff2532d00112', foundational, wavefunction_completeness_thesis).
narrative_ontology:cs_axiom_status(wavefunction_completeness_thesis, holdable).
narrative_ontology:cs_axiom_grounding('3fabacd6-87ed-4a3b-8f18-ff2532d00112', wavefunction_completeness_thesis, empirically_contingent).
narrative_ontology:cs_axiom('3fabacd6-87ed-4a3b-8f18-ff2532d00112', foundational, determinism_of_universal_dynamics).
narrative_ontology:cs_axiom_status(determinism_of_universal_dynamics, holdable).
narrative_ontology:cs_axiom_grounding('3fabacd6-87ed-4a3b-8f18-ff2532d00112', determinism_of_universal_dynamics, empirically_contingent).
narrative_ontology:cs_reference_frame('3fabacd6-87ed-4a3b-8f18-ff2532d00112', bare_unitary_universal_dynamics).
narrative_ontology:cs_drift_state('3fabacd6-87ed-4a3b-8f18-ff2532d00112', contemporary_post_decoherence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3fabacd6-87ed-4a3b-8f18-ff2532d00112', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everettian_foundations_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_cosmology_community).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, rival_interpretation_advocates).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, physics_graduate_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, decoherence_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universality_of_unitary_evolution).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_classicality_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers in the Everettian lineage build careers on deriving Born-rule weights, refining decoherence-based accounts of classicality, and defending branching ontology in journals and monographs. Program membership supplies citation networks, grant eligibility, and standing in the foundations subfield. Leaving would mean retraining into a different research identity; the program's claims are their professional self-description.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everettian_foundations_researchers, beneficiary,
    organized, generational, identity_locked, global).

% Applies quantum theory to the universe as a whole, where no external observer or classical apparatus exists to trigger collapse. The Everettian framework lets them write a wavefunction of the universe and treat structure formation as branching without invoking outside measurers. Alternative observer-free framings exist but none has comparable technical development, so switching frameworks means rebuilding toolchains.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_cosmology_community, beneficiary,
    organized, generational, constrained, global).

% Study environment-induced superselection and pointer states; the Everettian reading made their specialty load-bearing for interpreting quantum theory, channeling attention and funding toward them. The same prominence routes criticism to them whenever decoherence fails to fully deliver classicality or probability — they absorb the objection traffic the program generates. Their technical skills transfer readily to open-systems engineering and quantum error correction, so individual exit is easy even while the collective stake binds.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_researchers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, decoherence_researchers, payer).

% Defend collapse-type readings in the GRW lineage or hidden-variable readings in the Bohmian lineage. They publish against the grain of the ascendant framing, spend rebuttal effort on 'the problem is already solved' premises baked into referee reports and grant criteria, and watch textbook space close. Switching sides would dissolve decades of specialized work and public commitment; they stay and absorb the friction.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, rival_interpretation_advocates, payer,
    moderate, biographical, identity_locked, global).

% Encounter quantum foundations through curricula increasingly framed as 'measurement problem: addressed by decoherence,' inheriting a closed-seeming question set. Those with live doubts lack standing to reopen them and mostly route around foundations into application fields; the few who persist face visibility costs. Individual exit — leaving foundations altogether — is cheap and widely exercised.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, physics_graduate_students, payer,
    powerless, immediate, mobile, global).

% Design interferometry and optomechanics tests that could detect objective collapse and thereby bound branching ontology. Their research program sits outside the interpretive settlement's conversation — proposals framed as 'testing whether collapse happens' read as relics in venues premised on 'collapse is not a thing.' They would object that the settlement forecloses empirically decidable territory; they remain peripheral to it.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, collapse_model_experimentalists, excluded,
    moderate, biographical, constrained, global).

% Journal editors, textbook authors, and grant-panel members who decide which interpretive framings circulate: what counts as a solved problem, which derivations are referee-able, how chapters introduce measurement. They do not originate the Everettian program but administer its circulation, and can reprice their allegiance if the field's center shifts.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, foundations_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Audit the derivations and the ontology from outside program allegiance: formalize the probability problem, stress-test preferred-basis arguments, compare interpretive schemes for consistency. They neither collect nor fund the arrangement; their assessments feed back into the field as published critique.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, analytical_philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, everettian_foundations_researchers).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared framework in which quantum theory applies to the whole universe with no external observer and no collapse event: quantum cosmology writes a wavefunction of the universe, decoherence supplies the emergence of classical structure, and quantum-information foundations proceed on common unitary assumptions.
% TRANSFER_FUNCTION: Moves interpretive authority, citation traffic, grant eligibility, and textbook framing space toward the Everettian program's seats; moves the unresolved probability question onto future research agendas as a deferred cost borne diffusely by the field; moves rival framings toward the periphery of venues and curricula.
% ABSENT_VOICES: Collapse-model experimentalists would object that the settlement forecloses empirically decidable territory — their tests bound the very branching ontology the reading asserts — yet they sit outside the interpretive conversation. Operationalist practitioners who use the formalism daily without interpretation are likewise absent and would resist any settlement speaking in their name. Both absences mean the arrangement's unanimity is partial: the seats most able to test or ignore it were never in the room.
% DISAPPEARANCE_RATIONALE: Quantum cosmology loses its dominant observer-free framework overnight; the decoherence specialty loses its interpretive anchor; foundations venues and curricula reopen the measurement debate; careers priced in program membership revalue. The arrangement's absence would be felt within a publication cycle — it organizes real activity, not just opinion.
% FOUNDING_PROBLEM: Reconcile the measurement postulate with universal unitary dynamics: standard quantum mechanics invokes an external classical observer and a collapse process outside the Schrodinger equation, which cannot describe the universe as a whole. Everett's 1957 proposal was built to give quantum theory a formulation needing no outside measurer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: rival_interpretation_advocates and analytical_philosophers_of_physics attest the founding problem was and is real (the Bell-era critique of measurement, the ongoing motivation for collapse-model experiments) while disputing the program's claim that decoherence closes it. The historical record — von Neumann's measurement reformulation, Bohr's correspondence demands — independently attests the problem's reality. No source outside the program attests that the problem is solved; that attestation is internal, which is itself signal.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__many_worlds_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__many_worlds_reading),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.22, reading-indexed: assessed by the Everettian reading's own lights the arrangement extracts little — it claims to add no postulates and coerce no one — but it is not zero, because the reading itself concedes two burdens its arrangement imposes: the ontological extravagance every adherent must accept, and a probability problem the program declares closed faster than the derivations compel. Suppression is 0.34 and is authored as a raw structural property — it is NOT scaled by power or scope (only extractiveness is scaled, by directionality and scope, inside the engine). The suppression that exists is soft: editorial gatekeeping, grant framing, textbook closure — no coercion of persons. Theater is 0.38: genuine technical work (decoherence theory, decision-theoretic derivations, cosmological applications) runs beneath a victory-lap layer of 'the measurement problem is solved' rhetoric and popular amplification. Accessibility collapse is 0.62 rather than mountain-grade: inside the frame, alternatives collapse sharply (a collapse postulate is an addition; hidden variables are surplus ontology), but the contested Born-rule derivation keeps conceptual exits alive. Resistance is 0.58 — an organized critical tradition (probability-problem objections, preferred-basis disputes, collapse-test experimentalism) that has persisted for decades. Claim/metric independence: the mountain claim is what the reading asserts about itself; the metrics are what the arrangement's operation looks like; the engine measures the divergence. Boltzmann coordination_type is identity_coordination: the arrangement's primary coordination work is boundary-maintenance for a research community (what counts as taking the formalism seriously); the known gaming risk with identity framing is acknowledged — part of what travels under 'identity' is framing control — and coupling data should be checked for concentration on powerless agents at large scope. All three tracked series share one time grid (t=0,13,26,39,52,65) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The same sentence — 'decoherence solves the measurement problem' — lands differently by seat. To the beneficiary seats it is liberation: quantum cosmology finally has a framework with no external observer, and the decoherence specialty is promoted from niche to load-bearing. To the payer seats the same sentence is closure: rival advocates hear their research programs pronounced obsolete before refutation, and students inherit a curriculum where the question they found gripping is marked solved. Identity-lock deepens the gap: program membership is constitutive of the Everettian researcher's professional self ('taking the formalism seriously' as an identity marker), so exit would be self-dissolution rather than relocation; rival advocates are symmetrically locked into their lineages. If either identity frame broke, exit costs would drop, directionality would redistribute, and the arrangement's hold would loosen faster than its arguments change. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the program researchers collect the arrangement's returns directly (gain_flow names this seat); cosmologists receive the framework their field requires; decoherence researchers are dual-positioned — subsidized by prominence, taxed by objection traffic — placing them mid-range. Payers sit near the target end: rival advocates bear the marginalization with identity_locked exit, which amplifies their effective extraction toward the full-target pole; graduate students bear diffuse framing costs but their cheap individual exit dampens their position. Gatekeepers are near-symmetric: they administer whichever framing circulates and hold arbitrage-grade exit. Spatial scope is global throughout — verification of interpretive claims is maximally diffuse, which the engine's scope modifier registers on the extractive side. Suppression, again, enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — quantize the cosmos without an external classical observer — is live-contested, not dead: cosmology still needs an observer-free account, and the parties dispute whether decoherence delivers one. So no mandatrophy is declared. The classification nonetheless does preventive work in both directions. Against mislabeling as pure extraction: the arrangement's coordination function is real (a shared observer-free framework that cosmology and quantum information actually use), and the hybrid structure — beneficiaries, payers, soft enforcement — is kept visible rather than flattened into pure predation. Against laundering as natural law: the false-summit signature reads the declared beneficiaries against the naturality claim, so the arrangement cannot present advocacy-dependent persistence as physics. Watch-item: if decoherence were to settle decisively, the program could drift toward inertial maintenance — theatrical repetition of 'solved' claims after the live problem has moved elsewhere; the theater-ratio series is the early indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    everettian_naturality_vs_construction,
    'Is the Everettian arrangement a genuine consequence of the quantum formalism — a natural-law-like constraint no one chose and no one profits from — or a constructed interpretive settlement maintained by identifiable constituencies?',
    'Adversarial audit of the no-extra-postulates claim: enumerate every auxiliary assumption the arrangement requires in practice (branch individuation, weight measure, preferred-basis recovery, decision-theoretic axioms); if the count exceeds the formalism''s own content, the arrangement is constructed.',
    'If genuine, mountain certification stands and the declared beneficiaries are incidental; if constructed, the false-summit signature reclassifies toward tangled_rope and the beneficiary seats become the recipients of the arrangement''s returns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(everettian_naturality_vs_construction, conceptual, 'Natural-law vs constructed-status ambiguity of the Everettian arrangement (FSM omega).').

omega_variable(
    born_rule_derivation_status,
    'Do the Deutsch-Wallace decision-theoretic axioms establish branch weights as the unique rational credence measure, or does the probability problem remain open beneath the ''solved'' framing?',
    'Independent formal reconstruction of the derivation''s axioms (equivalence, diachronic consistency, branching indifference) with sensitivity analysis over weakened premises, adjudicated outside the program''s own venues.',
    'If the derivation holds, the arrangement''s central deflection is earned and measured extraction stays low; if it fails, the arrangement defers an unsolved problem while claiming closure, and measured extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_derivation_status, conceptual, 'Whether the probability problem is genuinely closed or rhetorically closed.').

omega_variable(
    decoherence_sufficiency,
    'Is decoherence sufficient to recover stable classical appearances and fix the preferred basis, or does it only suppress interference between coarse-grained records?',
    'Pointer-state robustness analyses across concrete models plus searches for metastable mesoscopic superpositions that survive environmental monitoring.',
    'Sufficient means the coordination function is solid and the beneficiary seats earn their returns; insufficient means the load-bearing mechanism weakens and the arrangement''s claims outrun its machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoherence_sufficiency, empirical, 'Adequacy of the decoherence mechanism the arrangement rests on.').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is the many_worlds_reading of kernel quantum_formalism; what exactly do the sibling readings (copenhagen_reading, pilot_wave_reading) change, and where is the disagreement located?',
    'Per-reading decomposition: locate the disputed element — whether the dynamics admits physical collapse, whether the wavefunction description is complete, whether measurement is primitive or derivative — and classify each sibling as its own constraint with its own epsilon.',
    'Classification is reading-indexed: adopting copenhagen_reading relocates costs into lost determinism and observer-privilege; adopting pilot_wave_reading relocates them into Lorentz-invariance tension and surplus ontology. Cross-reading comparison without per-reading epsilon is invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading among three of the quantum-formalism kernel.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression in the foundations community structural (editorial and funding gatekeeping) or internalized (early-career self-censorship that persists independent of the gates)?',
    'Publication-outcome audits controlling for paper quality, plus surveys of early-career researchers tracking stated willingness to pursue anti-Everettian framings versus actual submission behavior.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the gatekeeping with them after leaving gated venues — and the arrangement''s hold is stronger than its enforcement machinery shows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism ambiguity in the arrangement''s soft enforcement.').

omega_variable(
    extravagance_cost_allocation,
    'Does the infinite-worlds ontology impose a real cost on anyone — an opportunity cost crowding out rival research programs — or is it a free theoretical virtue?',
    'Comparative research-program accounting: funding and personnel flows to Everettian versus collapse-test and hidden-variable programs across the interval.',
    'Real cost means the declared victims are justified and the arrangement leans extractive; free virtue means the victim set shrinks and the arrangement trends toward pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extravagance_cost_allocation, preference, 'Whether ontological extravagance is a borne cost or a costless commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(quan_tr_t13, quantum_formalism__many_worlds_reading, theater_ratio, 13, 0.15).
narrative_ontology:measurement(quan_tr_t26, quantum_formalism__many_worlds_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement(quan_tr_t39, quantum_formalism__many_worlds_reading, theater_ratio, 39, 0.28).
narrative_ontology:measurement(quan_tr_t52, quantum_formalism__many_worlds_reading, theater_ratio, 52, 0.35).
narrative_ontology:measurement(quan_tr_t65, quantum_formalism__many_worlds_reading, theater_ratio, 65, 0.38).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(quan_be_t13, quantum_formalism__many_worlds_reading, base_extractiveness, 13, 0.12).
narrative_ontology:measurement(quan_be_t26, quantum_formalism__many_worlds_reading, base_extractiveness, 26, 0.16).
narrative_ontology:measurement(quan_be_t39, quantum_formalism__many_worlds_reading, base_extractiveness, 39, 0.19).
narrative_ontology:measurement(quan_be_t52, quantum_formalism__many_worlds_reading, base_extractiveness, 52, 0.21).
narrative_ontology:measurement(quan_be_t65, quantum_formalism__many_worlds_reading, base_extractiveness, 65, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(quan_su_t13, quantum_formalism__many_worlds_reading, suppression_requirement, 13, 0.1).
narrative_ontology:measurement(quan_su_t26, quantum_formalism__many_worlds_reading, suppression_requirement, 26, 0.18).
narrative_ontology:measurement(quan_su_t39, quantum_formalism__many_worlds_reading, suppression_requirement, 39, 0.24).
narrative_ontology:measurement(quan_su_t52, quantum_formalism__many_worlds_reading, suppression_requirement, 52, 0.3).
narrative_ontology:measurement(quan_su_t65, quantum_formalism__many_worlds_reading, suppression_requirement, 65, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'interpretation of quantum mechanics' decomposes into three structurally distinct constraints sharing the quantum_formalism kernel: this Everettian reading (measurement derivative, observer eliminable, epsilon 0.22 reading-indexed), copenhagen_reading (collapse as physical boundary process, indeterminism primitive), and pilot_wave_reading (definite positions guided by the wave). Each instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification; they are linked here as a constraint family. Upstream/downstream: the textbook Copenhagen settlement is historically upstream and is cited as the default this reading contests; this reading's decoherence machinery exerts downstream pressure on how the pilot-wave reading must justify its surplus ontology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
