% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living-Constitutionalist Reading: Adaptive Constitutional Interpretation
 *   domain: legal/political
 *
 * SUMMARY:
 *   The United States Constitution is a contested kernel: a single persisting
 *   text read differently by rival interpretive commitments. This story
 *   instantiates ONE reading — the living-constitutionalist reading, under
 *   which constitutional meaning evolves with society and interpretation
 *   legitimately adapts the text's principles to contemporary circumstances.
 *   As a constraint, the reading organizes an arrangement: a bench empowered
 *   to adapt principles, post-ratification practice and social change treated
 *   as authoritative inputs, and a professional apparatus that transmits the
 *   method. The arrangement solves a real coordination problem (an
 *   eighteenth-century text governing a transformed polity) while
 *   transferring final-say authority over contested social questions to an
 *   unelected, life-tenured bench — a transfer even this reading's own lights
 *   must acknowledge. The epsilon referent is the standing
 *   adaptive-interpretive arrangement as the living constitutionalist sees it
 *   — NOT the fixed-meaning arrangement the sibling reading would install.
 *   Constraint-family note: the colloquial label 'the Constitution'
 *   decomposes into at least three structurally distinct constraints (this
 *   reading, the originalist reading, the positivist reading), each with its
 *   own epsilon, beneficiary/victim sets, and classification; they are linked
 *   via network.affects_constraints, and the originalist sibling's resurgence
 *   is the drift vector recorded in cs_structure.drift_state. KEY AGENTS (by
 *   structural relationship): - federal_judiciary: agenda-setter and
 *   concentrated beneficiary (institutional/identity_locked) — administers
 *   the method and collects final-say authority -
 *   rights_claimants_changed_contexts: primary beneficiary
 *   (organized/constrained) — claims opened by adaptation, forum-dependent -
 *   civil_rights_advocacy_organizations: secondary beneficiary
 *   (organized/constrained) — converts adaptive rulings into durable doctrine
 *   - democratic_majorities_state_legislatures: primary payer
 *   (powerful/trapped) — bears override costs with no electoral exit -
 *   originalist_legal_scholars_movements: payer (moderate/identity_locked) —
 *   bears status and career costs; organized counter-movement -
 *   ratifying_generation: excluded party (powerless/trapped) — the displaced
 *   enacting public, absent by death - legal_historians: analytical observer
 *   — supplies the longitudinal record
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and concentrated beneficiary (institutional/identity_locked) — administers adaptive interpretation and accrues final-say authority
 *   - rights_claimants_changed_contexts: primary beneficiary (organized/constrained) — claims opened by adaptation, access routed through the bench
 *   - civil_rights_advocacy_organizations: secondary beneficiary (organized/constrained) — harvests adaptive rulings into durable doctrine
 *   - democratic_majorities_state_legislatures: primary payer (powerful/trapped) — bears override costs with no electoral exit from life tenure
 *   - originalist_legal_scholars_movements: payer (moderate/identity_locked) — bears status and career costs; organized into a counter-movement over the interval
 *   - ratifying_generation: excluded party (powerless/trapped) — the enacting public whose fixed understandings are displaced
 *   - legal_historians: analytical observer — documents the rise and fall of interpretive regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.5).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.35).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living-Constitutionalist Reading: Adaptive Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '5b07a3ac-8ba7-4f91-941f-426f62b627ed').
narrative_ontology:cs_kernel_codification('5b07a3ac-8ba7-4f91-941f-426f62b627ed', fixed_text).
narrative_ontology:cs_authority_grounding('5b07a3ac-8ba7-4f91-941f-426f62b627ed', practice).
narrative_ontology:cs_interpretation_layer_present('5b07a3ac-8ba7-4f91-941f-426f62b627ed').
narrative_ontology:cs_reading_relation('5b07a3ac-8ba7-4f91-941f-426f62b627ed', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b07a3ac-8ba7-4f91-941f-426f62b627ed', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('5b07a3ac-8ba7-4f91-941f-426f62b627ed', foundational, constitutional_principles_transcend_founding_applications).
narrative_ontology:cs_axiom_status(constitutional_principles_transcend_founding_applications, holdable).
narrative_ontology:cs_axiom_grounding('5b07a3ac-8ba7-4f91-941f-426f62b627ed', constitutional_principles_transcend_founding_applications, deontological).
narrative_ontology:cs_axiom('5b07a3ac-8ba7-4f91-941f-426f62b627ed', secondary, precedent_accumulation_is_authoritative).
narrative_ontology:cs_axiom_status(precedent_accumulation_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('5b07a3ac-8ba7-4f91-941f-426f62b627ed', precedent_accumulation_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('5b07a3ac-8ba7-4f91-941f-426f62b627ed', principles_charter_under_continuous_adaptation).
narrative_ontology:cs_drift_state('5b07a3ac-8ba7-4f91-941f-426f62b627ed', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5b07a3ac-8ba7-4f91-941f-426f62b627ed', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, democratic_majorities_state_legislatures).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, originalist_legal_scholars_movements).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, evolving_meanings_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, common_law_adaptation_method).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, representation_reinforcement_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the interpretive hierarchy: decides what the text's principles require now, binds lower courts through precedent, and reproduces itself through a confirmation process that screens for interpretive method. Each adaptive ruling converts a contested social question into a judicial holding and accrues final-say authority to the bench. Members cannot resign their way out of the method — their office, craft identity, and their institution's legitimacy are constituted by the practice of saying what the Constitution means.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, federal_judiciary, beneficiary).

% People asserting claims the founding generation never contemplated — same-sex couples seeking marriage recognition, patients seeking reproductive care, detainees challenging practices unknown in 1787. Adaptive interpretation opens constitutional doors that fixed-meaning methods close. Their access runs through the federal courts; when the bench's method shifts, their claims lose their forum.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts, beneficiary,
    organized, biographical, constrained, national).

% Movement infrastructure built around constitutional litigation strategies that presuppose evolving meaning: test cases, doctrinal sequencing, precedent accumulation. They convert adaptive rulings into durable doctrine. Their strategic repertoire is bound to the federal courts; state-constitution fallbacks exist but reach fewer people.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Enact laws reflecting their constituents' settled preferences and see them invalidated by evolving-interpretation rulings they cannot vote out — federal judges hold life tenure. Their exits are Article V amendment (supermajority-barred in practice), jurisdiction-stripping bills (of doubtful validity), or waiting for the bench's composition to change. They bear the override cost of every adaptive holding.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_majorities_state_legislatures, payer,
    powerful, generational, trapped, national).

% Academics, lawyers, and affiliated networks whose professional identity is constituted by the claim that meaning is fixed at ratification. Under adaptive-interpretation dominance they bore career and status costs: marginalization in casebook canons, clerkship hierarchies, and elite firm culture. Adopting the rival method would dissolve the commitment that defines them, so their exit is conversion, not relocation. Over the interval they organized — dedicated societies, judicial appointment pipelines — and converted status losses into institutional power.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, originalist_legal_scholars_movements, payer,
    moderate, generational, identity_locked, national).

% The enacting public whose fixed understandings adaptive interpretation displaces. Dead, they can object only through the texts they left — and this reading holds those texts underdetermine their own applications. They are the absent party whose consent-by-adaptation is asserted rather than obtained.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, ratifying_generation, excluded,
    powerless, generational, trapped, national).

% Document how interpretive regimes rose and fell — the Lochner sequence, the switch in time, incorporation, the Warren settlement — supplying the longitudinal record of the arrangement's operation. They neither collect from nor pay into it.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single operable constitutional framework across radical social and technological change: adaptation lets one eighteenth-century text supply settled rules for circumstances its drafters never faced, avoiding both perpetual amendment gridlock and disintegration into divergent state constitutional orders.
% TRANSFER_FUNCTION: Moves final interpretive authority over contested social questions from the enacting generation's fixed understandings and from contemporary legislative processes to the sitting federal bench; each adaptive holding converts a legislative or popular resolution into a judicial one, and precedent compounds the transfer.
% ABSENT_VOICES: The ratifying generation is absent by death and can object only through underdetermined texts. Contemporary citizens who lose adaptive rulings lack organized voice in the professional conversation that legitimates the method. For much of the interval, originalist-leaning jurists stood outside the elite gatekeeping conversation — casebooks, clerkships, firm hiring — that transmits interpretive legitimacy.
% DISAPPEARANCE_RATIONALE: If adaptive interpretation vanished overnight, every doctrine line built on it destabilizes at once: incorporation of the Bill of Rights against the states, equal-protection scrutiny tiers, substantive due process, commerce-clause breadth. Thousands of precedents and millions of reliance interests would collide with fixed eighteenth-century applications that cannot resolve present statutes; the modern rights architecture and the federal balance would both have to be renegotiated.
% FOUNDING_PROBLEM: A deliberately abstract eighteenth-century charter had to govern an industrializing, pluralistic continental republic across generations without perpetual amendment crises. The text underdetermined its own applications almost immediately — the national-bank controversy arrived within a generation of ratification — and someone had to be authorized to say what it requires now.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: originalist scholars attest the problem is live while disputing the solution — their entire program concedes the text underdetermines many questions and argues only over who resolves them; legal historians document the recurring underdetermination crises (the 1791 bank controversy, the 1937 court-plan crisis, the 1954–1973 integration sequence); comparative constitutional scholarship shows every long-lived written constitution develops an equivalent adaptive layer once amendment rates fall below the rate of social change.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.50: the arrangement's real coordination work (one operable framework across change) rides on a real transfer of final-say authority to an unelected bench; even by this reading's own lights the counter-majoritarian cost is acknowledged, which is why representation-reinforcement theory appears among the vindicated propositions. Suppression 0.35: alternatives are neither banned nor collapsed — originalism is published, taught, argued, and since the 1980s wins appointments and cases — but professional gatekeeping (canon formation, credentialing, confirmation screening) taxes deviation. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. Theater 0.31: a meaningful share of adaptive activity is dressed as discovery ('the text always meant this'), maintaining fidelity rhetoric over openly legislative adaptation. Accessibility_collapse 0.30: understanding the constraint does not close alternatives — the rival method is fully available, which is why resistance (0.62) is high and organized. Temporal design: ONE shared grid for the whole story, nine points at fifteen-year steps mapped onto 1900–2020, with all three tracked metrics authored at every point. Cyclical pattern: extraction oscillates with bench composition — a Lochner-era judicial-veto peak near t=0, a New Deal deference trough at t=45, a Warren/Burger peak at t=75, and backlash-era decline thereafter. The oscillation is currently a side effect of appointment politics rather than itself the extraction mechanism, though each swing raises the stakes of the next appointment (see omega oscillation_driver). The suppression_requirement series traces enforcement-capacity build-up — confirmation wars, gatekeeping intensification — peaking around t=90 and straining as the rival method captured appointing power.
 *
 * PERSPECTIVAL GAP:
 *   From the bench's seat the arrangement is a craft it mastered and an institution it constitutes — coordination built and maintained. From the trapped legislature seat the same structure operates as extraction by an actor it cannot vote out. From the identity-locked originalist seat it is status expropriation administered through credentialing. From the rights-claimant seat it is the only door. One structure, four computed experiences; the divergence is the datum the engine takes, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (rights claimants, advocacy organizations, the judiciary) drive d toward the subsidized end; victim declarations (state legislative majorities, originalist scholars) drive d toward the full-target end. Exit modulation sharpens the picture: legislatures are powerful but trapped — no electoral exit from life tenure — pushing them nearer full-target than their power alone would suggest; originalist scholars are identity_locked, since exit means dissolving the commitment that defines them, so they sit at the trapped end despite moderate power; the judiciary combines agenda-setting with direct collection, placing it nearest the beneficiary end; rights claimants are organized but forum-constrained, damped but not inverted. No directionality overrides were needed: the derivation from declared roles plus exit options reproduces these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — underdetermination recurs with each new technology and social arrangement — so no dead-mandate flag fires; the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie signature. The classification guards against two mislabels. Calling this a snare would erase the genuine coordination function: alternatives survive, compete, and win, so the persistence does not depend on suppressing exits the way predatory arrangements do. Calling it a rope would erase the documented asymmetric transfer: final-say authority accrues to a concentrated seat, which is why gain_flow names the bench rather than diffuse. Concentrated capturer plus live coordination function plus contested method is the tangled-rope signature, and the receipt surface records the capture half independently of the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of kernel us_constitution_text (reading: living_constitutionalist_reading); what structural deltas would the sibling readings instantiate?',
    'Compile and compare the sibling stories (us_constitution_text__originalist_reading, us_constitution_text__positivist_reading): flip the beneficiary/victim sets, relocate the epsilon referent, and recompute per-seat classifications under each reading''s own lights.',
    'Under the originalist sibling, rights claimants move from beneficiary to exposed and fixed-meaning advocates from payer to protected; under the positivist sibling the interpretive-authority transfer dissolves entirely and validity tracks enactment procedure. Cross-reading comparison is the corpus''s committer-axis measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling files carry the other readings.').

omega_variable(
    locus_of_meaning_authority,
    'Where does semantic authority over the constitutional text sit — ratification-time public understanding, enactment formality, or ongoing adaptive practice?',
    'Conceptual analysis of which authority each reading''s foundational axioms require, cross-checked against the sibling stories'' axiom declarations.',
    'Relocating authority flips which seats count as agenda_setter versus payer and rewrites the transfer_function; the disagreement between readings is located exactly here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locus_of_meaning_authority, conceptual, 'The specific structural element on which the kernel''s readings disagree.').

omega_variable(
    counter_majoritarian_override_rate,
    'How much democratic preference-override does adaptive interpretation actually impose, versus representation-reinforcing outcomes?',
    'Systematic coding of federal invalidation decisions against legislative-preference data across the interval.',
    'A high sustained override rate pushes effective extraction upward and drifts classification toward the snare end; a low rate supports the coordination-dominant framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_override_rate, empirical, 'Empirical magnitude of the counter-majoritarian cost the reading itself acknowledges.').

omega_variable(
    oscillation_driver,
    'Is the cyclical extraction pattern driven externally (bench composition follows ordinary political cycles) or internally (each adaptive swing raises the stakes of the next appointment — intermittent reinforcement)?',
    'Test whether swing amplitude grew over the interval after controlling for general political polarization; amplitude growth indicates internal reinforcement.',
    'If internally reinforced, the oscillation is itself an extraction mechanism and long-run effective extraction exceeds any single-period reading of the series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_driver, empirical, 'Driver of the observed extraction cycle across the 1900–2020 grid.').

omega_variable(
    transitional_arrangement_ambiguity,
    'Is adaptive interpretation a transitional bridge — the twentieth-century settlement awaiting a mature fixed-meaning jurisprudence — or a permanent feature of any durable-text polity?',
    'Comparative constitutional history: whether any long-lived written constitution has operated without an adaptive layer once amendment rates fall below the rate of social change.',
    'If transitional, the arrangement carries latent scaffold character and a sunset question; if permanent, persistence is structural and analysis centers on function maintenance rather than obsolescence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transitional_arrangement_ambiguity, conceptual, 'Whether the arrangement is inherently transitional or structurally permanent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__living_constitutionalist_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_text__living_constitutionalist_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_text__living_constitutionalist_reading, theater_ratio, 75, 0.36).
narrative_ontology:measurement_basis(us_c_tr_t75, observed).
narrative_ontology:measurement(us_c_tr_t90, us_constitution_text__living_constitutionalist_reading, theater_ratio, 90, 0.34).
narrative_ontology:measurement_basis(us_c_tr_t90, observed).
narrative_ontology:measurement(us_c_tr_t105, us_constitution_text__living_constitutionalist_reading, theater_ratio, 105, 0.33).
narrative_ontology:measurement_basis(us_c_tr_t105, observed).
narrative_ontology:measurement(us_c_tr_t120, us_constitution_text__living_constitutionalist_reading, theater_ratio, 120, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 45, 0.36).
narrative_ontology:measurement_basis(us_c_be_t45, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t75, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement_basis(us_c_be_t75, observed).
narrative_ontology:measurement(us_c_be_t90, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 90, 0.58).
narrative_ontology:measurement_basis(us_c_be_t90, observed).
narrative_ontology:measurement(us_c_be_t105, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 105, 0.54).
narrative_ontology:measurement_basis(us_c_be_t105, observed).
narrative_ontology:measurement(us_c_be_t120, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 120, 0.5).
narrative_ontology:measurement_basis(us_c_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 45, 0.24).
narrative_ontology:measurement_basis(us_c_su_t45, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t75, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 75, 0.34).
narrative_ontology:measurement_basis(us_c_su_t75, observed).
narrative_ontology:measurement(us_c_su_t90, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 90, 0.37).
narrative_ontology:measurement_basis(us_c_su_t90, observed).
narrative_ontology:measurement(us_c_su_t105, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 105, 0.36).
narrative_ontology:measurement_basis(us_c_su_t105, observed).
narrative_ontology:measurement(us_c_su_t120, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 120, 0.35).
narrative_ontology:measurement_basis(us_c_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel us_constitution_text per the epsilon-invariance principle: 'constitutional interpretation' is not one constraint. The living-constitutionalist reading (this file), the originalist reading, and the positivist reading instantiate different constraints with different epsilon, different victim sets, and different enforcement structures. Edges run from this story to both siblings. Downstream pressure: the originalist sibling's institutional ascent (appointment pipelines, the Dobbs-era repudiation of an adaptive-derived right) is the operating-environment change recorded in this reading's drift_state; upstream contribution: this reading's accumulated practice is the descriptive material the positivist sibling's rule-of-recognition registers. Each family member documents the decomposition; epsilon values are not comparable across readings because the referent shifts with each reading's own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
