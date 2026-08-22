% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Conception-Era Origin Dating of Digital Money
 *   domain: economic/historiographical/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'when did
 *   digital money emerge': the became_thinkable_reading, under which digital
 *   money emerged when the concept became technically and institutionally
 *   conceivable — the late-1970s/1980s corpus of cryptographic cash designs,
 *   electronic funds transfer architectures, and central-bank pilot proposals
 *   — prior to widespread implementation. The standing arrangement under
 *   contest (and the epsilon referent) is the historiographical convention
 *   that dates digital money's origin to that conception era and maintains it
 *   through peer review, curricula, archives, and commemoration. That
 *   convention performs a real ordering service for scholarship while
 *   simultaneously allocating priority credit, citation centrality, and
 *   archival investment to conception-era actors, and recording later
 *   implementers and informal digital-value practitioners as derivative or
 *   omitting them entirely. Per the kernel rules, the sibling readings
 *   (first-held, regulatory-recognition) are OTHER constraints in separate
 *   files; nothing about them is averaged into this story. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination function plus asymmetric extraction) while the metrics are
 *   authored independently from descriptive observation of the convention's
 *   operation.
 *
 * KEY AGENTS:
 *   - - early_institutional_architects: Primary beneficiary (organized/identity_locked) — conception-era cryptographers and central-bank researchers whose work the convention converts into founding acts
 *   - - canon_forming_monetary_historians: Agenda-setter and secondary beneficiary (institutional/identity_locked) — editors, textbook authors, and curators who administer the timeline and collect narrative authority from it
 *   - - implementation_era_builders: Primary target (powerful/constrained) — later deployers of digital payment systems recorded as realization rather than invention
 *   - - informal_digital_value_practitioners: Excluded target (powerless/trapped) — operators of unformalized digital value-transfer practices with no category in the conception-centered genealogy
 *   - - central_banks_and_monetary_authorities: Secondary beneficiary (institutional/arbitrage) — inherit a ready-made category that flatters institutional foresight
 *   - - science_technology_studies_scholars: Analytical observer (analytical/analytical) — studies the dating dispute itself and produces the revisionist histories the canon must answer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.52).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Conception-Era Origin Dating of Digital Money").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "economic/historiographical/technological").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '01735510-f7d9-4491-912f-f5884fae1464').
narrative_ontology:cs_kernel_codification('01735510-f7d9-4491-912f-f5884fae1464', distributed).
narrative_ontology:cs_authority_grounding('01735510-f7d9-4491-912f-f5884fae1464', distributed).
narrative_ontology:cs_reading_relation('01735510-f7d9-4491-912f-f5884fae1464', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('01735510-f7d9-4491-912f-f5884fae1464', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('01735510-f7d9-4491-912f-f5884fae1464', foundational, conception_era_designs_were_architecturally_complete).
narrative_ontology:cs_axiom_status(conception_era_designs_were_architecturally_complete, holdable).
narrative_ontology:cs_axiom_grounding('01735510-f7d9-4491-912f-f5884fae1464', conception_era_designs_were_architecturally_complete, empirically_contingent).
narrative_ontology:cs_axiom('01735510-f7d9-4491-912f-f5884fae1464', secondary, implementation_is_realization_not_invention).
narrative_ontology:cs_axiom_status(implementation_is_realization_not_invention, holdable).
narrative_ontology:cs_axiom_grounding('01735510-f7d9-4491-912f-f5884fae1464', implementation_is_realization_not_invention, conventional).
narrative_ontology:cs_reference_frame('01735510-f7d9-4491-912f-f5884fae1464', conceptual_origin_frame).
narrative_ontology:cs_drift_state('01735510-f7d9-4491-912f-f5884fae1464', post_bitcoin_post_mobile_money_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('01735510-f7d9-4491-912f-f5884fae1464', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, canon_forming_monetary_historians).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, implementation_era_builders).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_digital_value_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_banks_and_monetary_authorities).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conceptual_priority_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, inventor_centric_genealogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptographers, central-bank researchers, and standards contributors of the late 1970s and 1980s whose papers, patents, and pilot systems form the conception-era corpus. The dating convention records their work as the founding acts of digital money, bringing citation centrality, named priority, archive acquisition, and commemoration. Their professional identities and posthumous reputations are bound to the convention holding; a revised origin date would recast their contributions as preliminary sketches rather than foundations.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    organized, generational, identity_locked, global).

% Journal editors, textbook authors, doctoral advisors, and museum curators who maintain the shared timeline through peer review, syllabus design, and exhibition planning. They decide which origin candidates enter the record and which challenges count as footnotes. Their courses, surveys, and reference works are structured around the conception dating, and re-teaching the field around a different origin would strand much of their accumulated editorial and instructional capital.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, canon_forming_monetary_historians, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, canon_forming_monetary_historians, beneficiary).

% Engineers, product teams, and entrepreneurs who built deployed digital payment infrastructure decades after the conception era. The convention frames their systems as realization of an earlier conception, so their design decisions read as derivative and attribution flows to predecessors. They command resources to fund popular histories, oral archives, and corporate museums, but the peer-reviewed venues and curricula where the dating is reproduced remain effectively closed to them as authors of the origin story.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, implementation_era_builders, payer,
    powerful, biographical, constrained, global).

% Operators of money-movement practices that went digital outside formal banking: mobile-money agent networks, in-game currency markets, community ledger keepers, and remittance innovators. Several of these systems moved value digitally before or alongside official deployments, yet the conception-centered genealogy has no category for unformalized practice, so their innovations surface as anecdotes or vanish from the record. They hold no standing in the academic venues where the dating is defended and were never consulted in its formation.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_digital_value_practitioners, excluded,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, informal_digital_value_practitioners, payer).

% Monetary authorities and statistical agencies that inherited a ready-made category. Under the conception dating, digital money arrives as a long-anticipated object that authorities merely recognized and regulated, which supports narratives of institutional foresight and smooths the legitimation of later regulatory frameworks. Because they participate in many forums, they can invoke whichever periodization suits a given proceeding at low cost.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_banks_and_monetary_authorities, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers who study how technologies acquire origin myths. They examine the dating dispute itself, interview participants across all camps, and produce the revisionist and practice-centered histories that the canon must answer. They hold no stake in which date wins and can move freely among framings.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, science_technology_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared origin date and genealogy lets historians, economists, regulators, and technologists communicate about digital money's development, cite a common literature, structure curricula and museum narratives, and locate new developments on a stable timeline instead of relitigating basic periodization in every paper.
% TRANSFER_FUNCTION: Moves narrative authority, priority credit, citation centrality, and archival investment from later implementers and informal digital-value practitioners to conception-era architects and the canon-forming institutions that curate them.
% ABSENT_VOICES: Implementation-era builders are present economically but absent as authors in the peer-reviewed venues where the dating is reproduced; informal digital-value practitioners — mobile-money agents, gaming-economy participants, community ledger keepers — are absent entirely, having never been surveyed or invited into canon formation. Historians of everyday monetary practice would object that the genealogy erases use-side innovation, and they are largely outside the seminar rooms and editorial boards where the timeline is defended.
% DISAPPEARANCE_RATIONALE: If the conception-dating convention vanished overnight, priority claims and founder commemorations would dissolve, curricula and survey texts would require reperiodization, archive acquisition priorities would shift toward deployment-era and practice-era records, and the historiography would reorganize around whichever rival dating (first holding or regulatory recognition) captured the vacated anchoring role — the arrangement's beneficiaries and administrators demonstrably depend on it.
% FOUNDING_PROBLEM: The field needed order: by the 1980s-90s a confusing proliferation of monetary experiments (cryptographic cash proposals, electronic funds transfer pilots, stored-value cards, early online payment schemes) had to be sequenced into a teachable, citable history so that scholarship, regulation, and public memory could proceed.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist monetary historians and science-and-technology-studies scholars — outside the benefiting parties — corroborate that the ordering problem was real but attest that the conception-dating solution embedded priority politics from the start; museum archivists corroborate the practical need for some periodization while noting the acquisition bias it produces. No corroboration comes from the excluded practitioner communities themselves, because they were never consulted — which absence is itself signal about how the founding problem was framed.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end: the convention's yield is narrative and material rather than fiscal — priority credit, citation centrality, named 'founder' status, archive acquisition, and commemorative investment flow to conception-era actors, while implementers are recorded as derivative and informal practitioners are erased. Suppression is 0.52: enforcement is epistemic rather than coercive (peer-review gatekeeping, curriculum control, archive selection, review-of-challenge norms), but it is actively maintained and has intensified as revisionist pressure grew — hence the rising suppression_requirement series. Theater ratio is 0.32: anniversaries, founder commemorations, and 'father of digital cash' designations are performative layers on a still-real ordering function. Accessibility collapse is 0.40: alternative periodizations (implementation-first, user-practice, recognition-event histories) remain writable and partially published, so alternatives are dimmed, not eliminated. Resistance is 0.55: sustained revisionist scholarship, practitioner testimony, and funder-backed counter-histories meet the canon continuously. All three series share one six-point grid (t=0..45, roughly 1980-2025) so every metric is authored at every examined time point; the trajectories are monotonic, reflecting canon hardening rather than oscillation. Suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by the engine, from directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the architect seat the convention is earned recognition of genuine priority — the same structure reads as a memorial. From the builder seat it operates as attribution diversion: decades of engineering rendered footnote. From the historian-administrator seat it is neutral chronology, a mere ordering device — the seat least able to see its own rent collection because the rent is disciplinary authority. From the excluded practitioner seat the genealogy simply has no slot for them at all. The engine derives these divergent per-seat classifications from the power, exit, and role data below; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed. Early institutional architects sit nearest the beneficiary pole (declared beneficiaries, identity-locked exit — their legacies are fused with the convention, so they cannot arbitrage away from it). Canon-forming historians derive low directionality as declared beneficiaries despite administering the arrangement; their dual position (agenda_setter with secondary beneficiary role) is captured in the stakeholder roles rather than an override. Central banks derive mildly beneficiary directionality with arbitrage exit damping it further. Implementation-era builders derive high directionality as declared victims with constrained exit — they can fund counter-narratives but cannot leave the citation economy. Informal digital-value practitioners derive the highest directionality: declared victims, powerless, trapped, and additionally excluded from the venues where the dating is defended. The STS seat is analytical and takes no side. Larger-scope seats (global) face slightly amplified effective extraction under the engine's scope modifier, which matters most for the trapped regional practitioners whose erasure is hardest to verify from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — imposing teachable, citable order on a proliferating field of monetary experiments — was real and remains partly live, but the convention now allocates more credit than order: the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) is exactly the capture/zombie signature the R5 consumer cross-checks against the theater path. The hybrid classification prevents two opposite mislabels: reading the convention as pure extraction ignores the genuine coordination service (without a shared date, historiographical communication fragments); reading it as pure coordination ignores the measurable asymmetry (priority rents concentrate on identifiable actors while the excluded bear diffuse costs of erasure). Mandatrophy is not yet resolved — the ordering function still operates — but the trajectory (rising theater and suppression against a stable coordination base) marks the convention as accumulating extraction on top of a legitimate core, which is the structural condition the tangled_rope category exists to name.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (became_thinkable_reading) of the kernel digital_money_origin; how would the sibling readings (first_held_reading, regulatory_recognition_reading) alter the structural classification if instantiated as their own constraints?',
    'Generate the sibling stories as separate files and compare computed per-seat classifications and epsilon values across the family.',
    'If an implementation-based or recognition-based reading yields materially lower extraction, the measured extraction is specific to the conception-priority framing rather than to digital money''s history as such; if comparable, the extraction attaches to canon formation itself and survives across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three readings of the digital-money-origin kernel.').

omega_variable(
    architectural_completeness_dispute,
    'Were conception-era designs architecturally complete — did later deployment invent anything essential (decentralized consensus, trust-minimized settlement, scale-grade offline double-spend resistance) that the conception-era corpus lacked?',
    'Document-level comparison of conception-era specifications (cryptographic cash schemes, early electronic funds transfer architectures, central-bank pilot designs) against deployed systems, isolating elements with no conception-era antecedent.',
    'If essential elements demonstrably postdate conception, this reading''s foundational premise is empirically overridden, the origin date collapses toward first holding, and the beneficiary/victim structure inverts toward conception-era actors bearing the costs of a premature priority claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_completeness_dispute, empirical, 'Whether the conception-era corpus contained the essential architecture of digital money.').

omega_variable(
    coordination_vs_credit_allocation,
    'Does the shared conception-date primarily organize scholarly and regulatory communication about digital money, or primarily allocate priority credit to identifiable actors?',
    'Citation-network and curriculum analysis: measure whether abandonment of the convention would fragment communication or mainly redistribute attribution and archival resources.',
    'If credit allocation dominates, the coordination cover thins and the classification trends toward pure extraction; if communication dominates, the hybrid coordination-plus-extraction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_credit_allocation, empirical, 'Coordination function versus priority-rent allocation in the dating convention.').

omega_variable(
    exclusion_scope_bias,
    'Which practitioner communities fall outside the conceptual framing, and does their exclusion track the framing''s institutional and geographic biases?',
    'Comparative historiography covering informal and non-Western digital value practices (agent-network mobile money, gaming economies, community ledger systems, remittance innovations) against the conception-era canon.',
    'A wider excluded set raises effective extraction on those seats and strengthens the reading that the convention operates as boundary maintenance rather than neutral dating; a narrow excluded set supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_scope_bias, empirical, 'Scope and bias of the population excluded from the conceptual framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t9, digital_money_origin__became_thinkable_reading, theater_ratio, 9, 0.13).
narrative_ontology:measurement_basis(digi_tr_t9, observed).
narrative_ontology:measurement(digi_tr_t18, digital_money_origin__became_thinkable_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(digi_tr_t18, observed).
narrative_ontology:measurement(digi_tr_t27, digital_money_origin__became_thinkable_reading, theater_ratio, 27, 0.23).
narrative_ontology:measurement_basis(digi_tr_t27, observed).
narrative_ontology:measurement(digi_tr_t36, digital_money_origin__became_thinkable_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(digi_tr_t36, observed).
narrative_ontology:measurement(digi_tr_t45, digital_money_origin__became_thinkable_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement_basis(digi_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t9, digital_money_origin__became_thinkable_reading, base_extractiveness, 9, 0.41).
narrative_ontology:measurement_basis(digi_be_t9, observed).
narrative_ontology:measurement(digi_be_t18, digital_money_origin__became_thinkable_reading, base_extractiveness, 18, 0.49).
narrative_ontology:measurement_basis(digi_be_t18, observed).
narrative_ontology:measurement(digi_be_t27, digital_money_origin__became_thinkable_reading, base_extractiveness, 27, 0.55).
narrative_ontology:measurement_basis(digi_be_t27, observed).
narrative_ontology:measurement(digi_be_t36, digital_money_origin__became_thinkable_reading, base_extractiveness, 36, 0.59).
narrative_ontology:measurement_basis(digi_be_t36, observed).
narrative_ontology:measurement(digi_be_t45, digital_money_origin__became_thinkable_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement_basis(digi_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t9, digital_money_origin__became_thinkable_reading, suppression_requirement, 9, 0.28).
narrative_ontology:measurement_basis(digi_su_t9, observed).
narrative_ontology:measurement(digi_su_t18, digital_money_origin__became_thinkable_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement_basis(digi_su_t18, observed).
narrative_ontology:measurement(digi_su_t27, digital_money_origin__became_thinkable_reading, suppression_requirement, 27, 0.43).
narrative_ontology:measurement_basis(digi_su_t27, observed).
narrative_ontology:measurement(digi_su_t36, digital_money_origin__became_thinkable_reading, suppression_requirement, 36, 0.48).
narrative_ontology:measurement_basis(digi_su_t36, observed).
narrative_ontology:measurement(digi_su_t45, digital_money_origin__became_thinkable_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(digi_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'when digital money emerged' conflates three structurally distinct dating claims. This story (became_thinkable_reading) carries the conception-era dating with its own epsilon (0.62), its own beneficiary set (conception-era architects and canon-holders), and its own victim set (implementers and informal practitioners). The first_held_reading and regulatory_recognition_reading are separate files with different origin dates, different priority-rent recipients, and different excluded populations. This reading is upstream in the family: once adopted, its conception-era corpus becomes the authoritative origin record that the other two readings' operating environments draw on — the recognition reading inherits a longer antecedent arc, and the first-held reading must argue against a canon already anchored earlier. Cross-links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
