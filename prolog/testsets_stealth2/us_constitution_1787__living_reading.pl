% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living-Constitution Interpretive Regime (US Constitution, Living Reading)
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the living reading — of the
 *   contested kernel us_constitution_1787. The standing arrangement under
 *   contest is the operative interpretive regime in which constitutional
 *   meaning develops with society and the 1787 text functions as an
 *   aspirational framework whose open-ended clauses are filled by each
 *   generation's courts. Epsilon's referent is that standing arrangement,
 *   assessed by the reading's own lights: even sympathetic holders
 *   acknowledge a real transfer of settlement authority from democratic
 *   institutions to an interpretive elite, and the reading's own tradition
 *   names the risk that 'evolving norms' track professional-class opinion
 *   rather than society at large. Claim and metrics are independent authored
 *   facts: the arrangement is CLAIMED as tangled_rope because it solves a
 *   genuine coordination problem (keeping a hard-to-amend 18th-century text
 *   governable) while asymmetrically transferring settlement authority
 *   (identifiable payers) under active enforcement (compulsory compliance
 *   with doctrinal ceilings); the metrics describe its measured operation
 *   without tuning toward that claim. Sibling readings are separate
 *   constraint files linked through network.affects_constraints; the kernel
 *   contest is routed to omega variables, not folded into this
 *   classification. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setter and collector
 *   (institutional/identity_locked) — administers the regime and accumulates
 *   the authority it transfers - court_reliant_advocacy_networks: beneficiary
 *   (organized/mobile) — collects doctrinal wins and mission capital -
 *   legal_professoriate: beneficiary (organized/constrained) — supplies
 *   legitimating theory and trained personnel - electoral_majorities: primary
 *   target (organized/constrained) — legislative settlements reopened and
 *   decided elsewhere - state_governments: target (institutional/trapped) —
 *   bound beneath moving federal doctrinal ceilings -
 *   religious_conscience_minorities: target (powerless/constrained) —
 *   inherited practices ruled out of bounds -
 *   future_generations_bound_by_settlements: excluded voice
 *   (powerless/civilizational) - democratic_theorists: analytical observer —
 *   maps authorization and accountability
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and collector (institutional power, identity_locked exit) — administers the interpretive regime and accumulates the authority it transfers
 *   - court_reliant_advocacy_networks: beneficiary (organized, mobile) — collects doctrinal wins and mission capital through litigation
 *   - legal_professoriate: beneficiary (organized, constrained) — supplies legitimating theory and trains the personnel who apply it
 *   - electoral_majorities: primary target (organized, constrained exit) — legislative settlements reopened and decided by courts
 *   - state_governments: target (institutional, trapped) — compliance compulsory, no exit from federal supremacy
 *   - religious_conscience_minorities: target (powerless, constrained) — inherited practices displaced by updated interpretations
 *   - future_generations_bound_by_settlements: excluded voice (powerless, civilizational horizon) — bound by settlements authored before they exist
 *   - democratic_theorists: analytical observer — analyzes legitimacy without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.48).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.52).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living-Constitution Interpretive Regime (US Constitution, Living Reading)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'f27e9301-52c6-4f5c-a433-3d46f6f3cc19').
narrative_ontology:cs_kernel_codification('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', fixed_text).
narrative_ontology:cs_authority_grounding('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', practice).
narrative_ontology:cs_interpretation_layer_present('f27e9301-52c6-4f5c-a433-3d46f6f3cc19').
narrative_ontology:cs_reading_relation('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', foundational, constitutional_meaning_tracks_social_evolution).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_social_evolution, holdable).
narrative_ontology:cs_axiom_grounding('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', constitutional_meaning_tracks_social_evolution, deontological).
narrative_ontology:cs_axiom('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', foundational, text_aspirational_framework_open_ended).
narrative_ontology:cs_axiom_status(text_aspirational_framework_open_ended, holdable).
narrative_ontology:cs_axiom_grounding('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', text_aspirational_framework_open_ended, conventional).
narrative_ontology:cs_reference_frame('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', aspirational_evolutionary_framework).
narrative_ontology:cs_drift_state('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f27e9301-52c6-4f5c-a433-3d46f6f3cc19', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, court_reliant_advocacy_networks).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, legal_professoriate).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, electoral_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_governments).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, religious_conscience_minorities).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, constitutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured judges and justices decide what the Constitution requires now. Each generation of appointees extends or revises doctrinal settlements in light of contemporary conditions, and precedent authored today becomes the floor for tomorrow. Leaving is effectively unavailable: tenure binds them to the institution, and their professional standing is constituted by the interpretive craft itself.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, federal_judiciary, beneficiary).

% Public-interest legal organizations litigate for expanded rights settlements they cannot win legislatively. Wins flow to them as mission fulfillment and fundraising capital; losses redirect strategy. They can and do shift among courts, legislatures, and state arenas, so their position is strategic rather than captive.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, court_reliant_advocacy_networks, beneficiary,
    organized, biographical, mobile, national).

% Academic lawyers supply the theories (privacy, dignity, evolving standards) that justify updating, train the clerks and judges who apply them, and collect scholarly authority from the framework's continued centrality. Some members defect to rival methodologies, so exit exists but carries career cost within elite institutions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legal_professoriate, beneficiary,
    organized, biographical, constrained, national).

% Voting publics find policy questions they settled legislatively reopened and decided by courts: punishment, privacy, religious accommodation, economic regulation. Their recourse runs through slow channels — presidential appointments confirmed by senators, occasional amendments, jurisdiction-curbing statutes. None of these removes an existing settlement quickly.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, electoral_majorities, payer,
    organized, generational, constrained, national).

% State legislatures and state constitutions operate beneath federal doctrinal ceilings that move without their consent. Compliance is compulsory — federal courts command state officers — and secession or nullification is unavailable, so their levers are litigation strategy and political pressure on the appointment process.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_governments, payer,
    institutional, biographical, trapped, regional).

% Communities whose moral settlements predate current rights doctrine find long-standing practices ruled out of bounds by updated interpretations. They lack the numbers or resources to reshape appointments quickly and experience each doctrinal revision as a loss imposed without a forum they could win.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, religious_conscience_minorities, payer,
    powerless, generational, constrained, national).

% People not yet born inherit doctrinal settlements authored by current courts under a theory that presumes to speak for them. They have no seat anywhere in the process; their interests enter only as rhetorical invocations inside opinions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, future_generations_bound_by_settlements, excluded,
    powerless, civilizational, trapped, continental).

% Political theorists and constitutional scholars outside the litigation stream analyze the arrangement's legitimacy — mapping who authorizes interpretive change and who answers for it. They collect no settlement and bear none; their output is critique and institutional design proposals.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_theorists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a short, deliberately hard-to-amend founding text operable across two centuries of transformed circumstances: provides one shared, continuous mechanism for determining what supreme law requires now, avoiding both paralysis-by-rigidity and perpetual supermajority amendment warfare.
% TRANSFER_FUNCTION: Moves interpretive authority over the content of supreme law from the Article V amendment process and elected institutions to courts and the professional legal class; concretely, moves individual policy settlements (punishment, privacy, religious accommodation, equality) from legislative determination to judicial determination.
% ABSENT_VOICES: Citizens who hold that constitutional change belongs exclusively to Article V have no institutional seat inside the interpretive regime — their objection registers only as electoral pressure on appointments. Future generations bound by today's doctrinal settlements are absent entirely. Both stand outside the courtroom in which the arrangement's legitimacy is produced.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — courts reverting to fixed-text adjudication — the incorporated rights architecture, the substantive due process line, evolving-standards punishment doctrine, and decades of precedent built on updated meaning would destabilize simultaneously; millions of reliance interests would reopen; national politics would reorganize around a vastly enlarged amendment and court-curbing agenda.
% FOUNDING_PROBLEM: How to keep an 18th-century charter authoritative and workable for a nation its framers did not anticipate — an industrial economy, national media, new technologies, transformed moral understandings — without either abandoning the charter or freezing it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: originalist scholars concede the ratified text underdetermines modern cases even while rejecting this remedy; comparative jurisdictions (Canada's living-tree doctrine, 1929) developed parallel adaptation mechanisms with no US judicial self-interest at stake; the amendment record itself — 27 amendments in 235 years, most procedural or franchise-extending rather than re-plumbings of governance — attests the rigidity problem the arrangement addresses.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.48 because the arrangement's core operation is a real transfer — policy settlements move from legislatures to courts — bounded by the genuine service the transfer performs. Suppression is 0.52 and structural, not internalized: compliance with doctrinal ceilings is compelled by ordinary federal legal authority (command to state officers, contempt, injunction), with no exit-suppression apparatus comparable to predatory arrangements; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change — machinery built up across the twentieth century (consolidated judicial supremacy, law-school canonization, the clerkship pipeline) now straining under legitimacy attack, state testing of unpopular rulings, and court-curbing proposals, hence the rise to 0.57 and fall to 0.52. Theater_ratio is 0.46 and rising honestly: ceremonial fidelity-to-text rhetoric over result-driven evolution, nominees disclaiming the method they then practice, 'evolving standards' citations that track elite opinion polling — below the 0.5 proxy-substitution line but trending toward it. Accessibility_collapse is low (0.25) because nothing collapses: originalism and textualism remain fully live rivals, and Article V stays open. Resistance is high (0.65): appointment wars are the resistance mechanism, plus a scholarly counter-movement and state-level pushback. The extractiveness series oscillates rather than climbs monotonically — the driver is the appointment cycle, which alternates the DIRECTION of elite capture (progressive settlements, then conservative ones) without reducing the magnitude of the authority transfer itself; the oscillation is a side effect of external political cycles, not an intermittent-reinforcement mechanism. Coalition potential among payers is real and partially realized: electoral majorities and state governments converge in the Article V convention movement and court-curbing legislation, which is why resistance reads high despite dispersed payer power.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently. From the bench, the arrangement is trusteeship it staffed and believes in — continuity, rights protection, orderly adaptation; from the constrained payer seats the same structure operates as settlement authority taken without consent, with exit running only through decade-scale appointment and amendment channels. The judiciary's identity_locked exit matters here: professional identity is fused with the interpretive vocation, so the seat cannot price its own position neutrally, and if the legitimacy frame broke (widespread open non-compliance with unpopular rulings), the arrangement's enforcement would degrade faster than its doctrine could adapt. State governments sit at the same nominal power atom as the federal bench but differ entirely in exit: trapped beneath federal supremacy versus structurally indispensable to it — a same-level asymmetry the engine reads from exit options, not titles.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The federal judiciary is declared beneficiary (with agenda-setting secondary) — it collects the transferred authority, so its directionality sits near the beneficiary end despite administering the regime. Advocacy networks and the professoriate are beneficiaries with mobile or constrained exits, damping their effective extraction further. Electoral majorities are targets with only slow-channel recourse; state governments are targets with no exit at all, pushing them toward the full-target end; religious conscience minorities combine target position with powerlessness, the highest effective extraction among the seated payers. Future generations are authored as excluded, not as a directional seat: an authored absence is commentary-grade only and never drives a classification override — it feeds the consensus-provenance check (the regime's internal unanimity is produced in a room those seats never entered). No directionality overrides are used: the structural derivation from roles, power, and exits captures every seated relationship, and the override surface keys on power atoms that would collide across differently-positioned institutional actors here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adapting a rigid charter to unforeseen circumstances — is live, not dead: new technologies and social forms keep arriving, and the corroboration record (comparative living-tree doctrines, the amendment-gap record, originalist concessions about textual underdetermination) comes substantially from outside the benefiting parties. The mismatch consumer therefore reads status=live x verdict=world_rearranges: no zombie flag, no mandatrophy declaration. The classification guards against mislabeling in both directions: reading the arrangement as pure extraction ignores the coordination function that keeps a 235-year-old supermajority-amended text governable at all; reading it as pure coordination ignores that the transfer's losers never consented to judicial authorship of supreme law and cannot reverse settlements through any fast channel. The tangled_rope claim holds both facts in one structure, which is what the seat-divergent computation exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_norms_provenance,
    'Are the ''evolving norms'' this arrangement incorporates genuinely societal consensus, or professional-class elite opinion filtered through the legal pipeline?',
    'Compare the timing of doctrinal settlements against mass-opinion and elite-opinion trajectories on the same questions (capital punishment, privacy, religious accommodation); if settlements systematically lead mass opinion and track elite opinion, the incorporated norms are elite-authored.',
    'If elite-authored, the transfer takes settlements from electoral majorities and hands them to a narrow class, raising effective extraction above the measured 0.48 and pushing payer-seat classifications toward the extractive end; if genuinely societal, the arrangement''s coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_norms_provenance, empirical, 'Whether incorporated ''evolving norms'' are societal or elite in origin').

omega_variable(
    kernel_reading_pluralism,
    'This constraint is one reading of kernel us_constitution_1787 (the living_reading); the originalist_reading and positivist_reading instantiate different constraints with different victim sets and different epsilon values — does the kernel contest resolve toward one reading, or is it a permanent pluralism in which all three remain live?',
    'Track the composition of the bench, the law-school canon, and the profession''s methodological self-description across generations; resolution toward a single reading would show as methodological monoculture in appointments and doctrine.',
    'If the originalist reading displaces this one, the victim set inverts — the payers become those reliant on unenumerated-rights settlements — and this file''s entire beneficiary/victim structure flips; permanent pluralism keeps the current structure with contested enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_pluralism, conceptual, 'Committer structure: sibling readings of the 1787 kernel and what their ascendancy would change').

omega_variable(
    counter_majoritarian_valence,
    'Is the transfer of settlement authority from elected institutions to courts a democratic deficit (extraction) or a trusteeship that protects constitutional commitments from transient majorities (service)?',
    'Not resolvable by data alone: the answer depends on prior commitments about where constitutional authority legitimately resides — this is the live normative core of the kernel contest itself.',
    'If deficit, the transfer function reads as the arrangement''s principal extraction vector; if trusteeship, the same flow reads as the price of the coordination service, and measured extraction drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_valence, preference, 'Value-dependent reading of the democratic-to-judicial authority transfer').

omega_variable(
    enforcement_capacity_trajectory,
    'Is the recent erosion of enforcement capacity (legitimacy attacks, state testing of unpopular rulings, court-curbing proposals) a cyclical trough or a secular decline?',
    'Track compliance rates with unpopular federal rulings, court-curbing and jurisdiction-stripping bills, and appointment-norm stability over the coming decades.',
    'Secular decline converts the arrangement toward theatrical maintenance of settlements it can no longer reliably enforce; cyclical recovery restores the stable tangled_rope profile with the post-1965 enforcement plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Whether the suppression_requirement downturn after 2008 is cyclical or secular').

omega_variable(
    method_output_locus,
    'Is the extractive element located in the evolutionary METHOD itself, or only in particular doctrinal OUTPUTS (specific rights settlements) that the method happens to have produced?',
    'Counterfactual analysis: if the same method had produced only settlements matching sustained legislative majorities, would the identified payer seats still object? Decompose payer objections into method-objectors and outcome-objectors.',
    'If extraction lives in the method, epsilon attaches to this constraint as authored; if only in outputs, the correct decomposition writes separate stories per doctrinal line (each with its own epsilon and victim set) and this file''s epsilon falls toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_output_locus, conceptual, 'Whether extraction attaches to the interpretive method or to specific doctrinal products').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1900, us_constitution_1787__living_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.18).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_1787__living_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__living_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_1787__living_reading, theater_ratio, 1992, 0.36).
narrative_ontology:measurement(us_c_tr_t2008, us_constitution_1787__living_reading, theater_ratio, 2008, 0.4).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__living_reading, theater_ratio, 2025, 0.46).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1900, us_constitution_1787__living_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.36).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.46).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_1787__living_reading, base_extractiveness, 1965, 0.54).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__living_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_1787__living_reading, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement(us_c_be_t2008, us_constitution_1787__living_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__living_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1900, us_constitution_1787__living_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.26).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.38).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_1787__living_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__living_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_1787__living_reading, suppression_requirement, 1992, 0.56).
narrative_ontology:measurement(us_c_su_t2008, us_constitution_1787__living_reading, suppression_requirement, 2008, 0.57).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__living_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what the Constitution requires' covers three structurally distinct claims, written as three files. This file instantiates the living reading (meaning evolves; text aspirational). The originalist reading (meaning fixed at ratification) and the positivist reading (text plus democratic amendments) are separate constraints with their own epsilon values, victim sets, and classifications — each reading authors its own epsilon over the standing interpretive arrangement by its own lights. Edges run both ways because the readings compete for the same enforcement infrastructure: originalist ascendancy on the bench drains this reading's enforcement capacity, while entrenched living-reading precedent raises the cost of any originalist displacement. Contamination propagates across the family through appointment politics and legitimacy capital.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
