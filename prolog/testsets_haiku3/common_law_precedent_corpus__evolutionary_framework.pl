% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Normative Framework
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   Common law precedent under the evolutionary-framework reading treats
 *   prior judicial decisions as the binding foundation of legal order, but
 *   empowers courts to reinterpret and overrule precedent when contemporary
 *   normative understanding evolves, social conditions shift, or prior
 *   reasoning is exposed as flawed. This reading sits in active contest with
 *   strict stare decisis (which treats precedent as backward constraint
 *   requiring extraordinary justification) and pluralist balancing (which
 *   contextualizes precedent weight by domain). The evolutionary reading
 *   emphasizes that law must be responsive to justice and constitutional
 *   development, legitimating doctrinal change as corrective rather than
 *   radical. Its operation creates extractive asymmetries: progressive
 *   litigants and academics gain voice and pathway to reshape doctrine;
 *   conservative reliance interests and non-litigant publics bear the cost of
 *   doctrinal instability. The constraint is claimed as rope (genuine
 *   coordination through stable-yet-responsive precedent) but measured as
 *   substantially extractive (0.38 base extractiveness), moderately
 *   suppressive (0.22), and moderately theatrical (0.18). The measurement
 *   trajectory shows extractiveness rising sharply to t=25 then plateauing —
 *   this reflects the accelerating doctrinal shifts of the civil-rights and
 *   constitutional-development eras (t=0-25) followed by a period of
 *   doctrinal stabilization at a higher baseline (t=25-40). This reading
 *   vindicates three propositions: that law is responsive to social change,
 *   that the judiciary holds normative authority, and that precedent
 *   functions as a living tradition.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Institutional agenda-setter. Interprets and applies the reinterpretation authority that the evolutionary reading confers. Holds the gate through which normative evolution becomes doctrine.
 *   - progressive_litigants: Moderate power, mobile exit. Beneficiaries of pathways to challenge settled doctrine. Their litigation strategy is reframing — arguing evolution rather than rupture.
 *   - conservative_litigants: Moderate power, mobile exit. Payers of doctrinal instability. Rely on precedent as backward constraint but see it eroded by evolutionary reinterpretation.
 *   - legal_academics: Organized, arbitrage exit. Beneficiaries. Generate the narratives of normative progress and doctrinal genealogy that justify evolution. Their scholarship feeds into judicial reasoning.
 *   - constitutional_traditionalists: Organized, constrained exit. Payers. Defend older doctrinal settlements and dispute the normative narratives used to justify evolution.
 *   - non-litigant publics: Powerless, trapped exit. Excluded from the interpretive process. Experience doctrinal shifts as fait accompli.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.22).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Normative Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '594aae40-425d-4dfd-9fb2-fbaf6903b6f1').
narrative_ontology:cs_kernel_codification('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', distributed).
narrative_ontology:cs_authority_grounding('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', lineage).
narrative_ontology:cs_interpretation_layer_present('594aae40-425d-4dfd-9fb2-fbaf6903b6f1').
narrative_ontology:cs_reading_relation('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', foundational, precedent_responsive_to_normative_evolution).
narrative_ontology:cs_axiom_status(precedent_responsive_to_normative_evolution, holdable).
narrative_ontology:cs_axiom_grounding('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', precedent_responsive_to_normative_evolution, deontological).
narrative_ontology:cs_axiom('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', foundational, judiciary_as_constitutional_interpreter).
narrative_ontology:cs_axiom_status(judiciary_as_constitutional_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', judiciary_as_constitutional_interpreter, deontological).
narrative_ontology:cs_reference_frame('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', precedent_as_living_tradition).
narrative_ontology:cs_drift_state('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', civil_rights_constitutional_development_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('594aae40-425d-4dfd-9fb2-fbaf6903b6f1', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, progressive_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, constitutional_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_academics).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, conservative_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, constitutional_traditionalists).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, law_responsive_to_social_change).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, judicial_normative_authority).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, precedent_as_living_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies precedent, with authority to distinguish, limit, or overrule prior holdings when contemporary normative understanding evolves. This reading empowers courts to frame doctrinal reinterpretation as corrective refinement rather than radical rupture. The judiciary holds the interpretive apparatus that determines whether a precedent remains binding or has been superseded by normative development.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Seek to challenge or reframe settled precedent on grounds that social understanding has evolved, constitutional values have deepened, or prior doctrine was founded on flawed premises now exposed. Under this reading, they have viable pathways to argue for doctrinal evolution rather than being foreclosed by backward constraint. Their litigation success depends on convincing courts that evolution is justified.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, progressive_litigants, beneficiary,
    moderate, biographical, mobile, national).

% Rely on stable precedent to anchor their rights and expectations. Under the evolutionary reading, they bear the cost of doctrinal instability — what they believed was settled law may be reinterpreted or overruled if courts find normative evolution justifies it. Their exit option is to litigate the boundary between evolution and rupture, but the outcome is not guaranteed.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, conservative_litigants, payer,
    moderate, biographical, mobile, national).

% Must apply precedent while monitoring appellate signals about which doctrines are stable and which are in doctrinal flux. Under the evolutionary reading, they gain interpretive flexibility but also face uncertainty about which precedents appellate courts will preserve or evolve. Their decision-making space expands but becomes less deterministic.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_court_judges, observer,
    powerful, biographical, constrained, national).

% Generate interpretive frameworks justifying doctrinal evolution. Under this reading, scholarship that demonstrates normative progress or identifies flawed premises in prior doctrine becomes a primary input to legitimacy. Academics gain authority as architects of doctrinal reframing and normative genealogy.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_academics, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, legal_academics, observer).

% Defend older doctrinal settlements and dispute the normative narratives invoked to justify evolution. They pay the cost of doctrinal instability and the delegitimization of frameworks they regard as well-settled. Their options are legislative reversal or appellate resistance, both costly and uncertain.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, constitutional_traditionalists, payer,
    organized, generational, constrained, national).

% Third parties affected by doctrinal shifts but not parties to litigation that would trigger reinterpretation. They experience doctrinal evolution as fait accompli, without voice in the process that determined which precedents would evolve and which would hold.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, excluded_non_litigants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared normative framework through precedent that is responsive rather than calcified: coordinates judicial action around principles that can be reinterpreted as constitutional understanding deepens and social conditions evolve, without requiring formal amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from text-as-written toward judiciary-as-normative-arbiter. Progressive litigants and academics gain pathways to challenge settled doctrine; conservative reliance interests pay the cost of doctrinal instability.
% ABSENT_VOICES: Non-litigant third parties affected by doctrinal shifts (the incarcerated whose precedent-based rights change, workers whose employment doctrine evolves, citizens whose privacy precedents shift) are excluded from the interpretive process. Their exposure to doctrinal change is passive and unvoiced. Legislative representatives might challenge doctrinal trajectories but do not participate in the judicial reinterpretation itself.
% DISAPPEARANCE_RATIONALE: If the evolutionary reading vanished and were replaced by strict backward constraint, the judiciary would lose authority to reframe doctrine as corrective evolution; precedent would require extraordinary justification to depart from; and litigants would be foreclosed from challenging settled doctrine on normative grounds. The entire appellate docket would reorganize around distinguishing rather than reframing, and constitutional doctrine would calcify around whatever settlement obtained when the constraint shifted.
% FOUNDING_PROBLEM: The founding problem was to establish a legal order that could bind present decisions through precedent (coordination, stability) while remaining capable of correction when constitutional understanding deepened or social reality shifted (responsiveness, justice). Precedent-as-binding without capacity for normative evolution produces injustice; precedent-as-infinitely-revisable produces legal uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Progressive legal scholars and civil-rights litigants attest that the founding problem persists: courts must balance stability and justice, and evolutionary reinterpretation is the primary mechanism for correcting doctrinal mistakes. Conservative scholars and originalists dispute whether the founding problem is solved through evolution or generated BY permitting it; they attest that strict constraint was the intended solution. No neutral external corroboration exists — the dispute is internal to jurisprudence and turns on whether social evolution warrants doctrinal change.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at plateau) because the evolutionary reading creates asymmetric power to reshape doctrine: progressive litigants and academics have viable pathways to challenge precedent; conservative reliance interests and non-participant publics bear the cost of instability without voice in the process. The constraint is not purely extractive (like a snare) because there is genuine coordination value — the judiciary does maintain a shared normative framework through precedent, and courts do limit their reinterpretation to reasoned genealogy rather than arbitrary reversal. Suppression is low (0.22) because the evolutionary reading explicitly permits and normalizes doctrinal challenge — there is no enforcement machinery preventing litigants from arguing that precedent should evolve. Theater is low (0.18) because the constraint's operation is largely transparent: when courts reframe doctrine, they explain the evolution explicitly. The measurement series capture the trajectory of the civil-rights era (t=0-25, rising extractiveness) when courts actively reinterpreted precedent on race, gender, and constitutional rights, followed by a plateau (t=25-40) as the core doctrinal revisions stabilized and the rate of fundamental reframing slowed. The theatrical ratio rose slightly during the period of active reframing (as courts justified changes in increasingly elaborate terms) then held steady at a low level.
 *
 * PERSPECTIVAL GAP:
 *   The appellate-judiciary and progressive-litigant seats should compute as beneficiaries (they gain authority to reshape doctrine). The conservative-litigant and traditionalist seats should compute as payers (their reliance interests face instability). Non-litigant publics should compute as excluded (they experience change without voice). The directionality derivation from beneficiary/victim data should produce low d for the beneficiary seats and high d for the payer seats, generating the expected per-seat type divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The evolutionary reading concentrates interpretive authority in the hands of appellate judges, progressive scholars, and litigants whose normative position aligns with evolution. Conservative reliance interests and constitutionalists who prize stability pay the cost of doctrinal instability — they must constantly monitor whether their understanding of settled law remains settled, and litigate to defend it if courts signal reinterpretation. This asymmetry flows from the reading's core claim: that law must be responsive to normative development. Under strict stare decisis (the sibling reading), the directionality would invert — conservative reliance interests would be the beneficiaries (precedent as backward constraint protects their settlement), and progressive reformers would be the payers (foreclosed from doctrinal challenge). The evolutionary reading does not suppress litigation; it enables and normalizes doctrinal challenge. So suppression is low. What is asymmetric is authority and benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuine and live: courts must balance precedent as binding foundation with precedent as capable of correction. The evolutionary reading solves it by empowering courts to reinterpret as normative understanding evolves. This is not mandatrophy — the mandate (respond to constitutional development) has not outlived its function. However, there is a secondary stability question: if courts exercise the power to evolve doctrine too readily, or if the narrative of evolution becomes a cover for ideological remaking, the constraint could degrade into a snare (extractive reinterpretation masked as evolution). The theater-ratio trajectory (rising to t=25 then plateauing) suggests courts invested in explaining evolution explicitly during the high-change period, then settled into a lower-theater baseline once core doctrine stabilized. This indicates the constraint is not performing as pure theater — the evolution was real, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_evolution_vs_ideology,
    'When courts invoke normative evolution to justify doctrinal change, are they identifying genuine progress in constitutional understanding, or rationalizing ideological preferences?',
    'Comparative study of doctrinal reinterpretation across different judicial eras and coalitions: if evolution rhetoric is applied asymmetrically (defending some doctrinal shifts as evolved while dismissing others as radical), the mechanism is ideological cover; if applied consistently across conservative and progressive doctrine, the mechanism is genuinely responsive to understanding.',
    'If ideological, the constraint reclassifies toward snare (extractive power masked as evolution). If genuine, the rope classification holds and the asymmetry is the price of responsiveness rather than corruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_evolution_vs_ideology, empirical, 'Whether doctrinal evolution is normative progress or ideology masked as progress.').

omega_variable(
    boundary_between_evolution_and_rupture,
    'What differentiates legitimate doctrinal reinterpretation (evolution) from illegitimate break with precedent (rupture)? Where does courts'' authority to evolve doctrine end?',
    'Examination of appellate jurisprudence on overruling: which doctrinal reinterpretations are framed as evolution and which as rupture, and what principled distinction exists between them. If the boundary is internal to the judiciary''s reasoning (courts decide case-by-case whether evolution is justified), the reading has no external limit. If the boundary is enforced externally (legislative veto, constitutional amendment requirement), the reading is constrained.',
    'If boundary is internal only, the evolutionary reading grants open-ended interpretive authority to courts; if external constraints operate, the reading''s scope is limited. This affects whether the constraint is genuinely a coordination mechanism (bounded evolution) or extraction machinery (unbounded reinterpretation authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_between_evolution_and_rupture, conceptual, 'Where the authority to reinterpret doctrine ends and usurpation begins.').

omega_variable(
    kernel_contest_reading_coexistence,
    'Can the evolutionary_framework reading and the strict_stare_decisis reading coexist in a single legal system, or does adoption of one foreclose the other?',
    'Historical study of common-law jurisdictions: some courts and periods emphasize evolutionary reinterpretation; others emphasize strict backward constraint. If both coexist (different panels, different domains, oscillating emphasis), the readings coexist_with each other. If courts adopting evolutionary reasoning systematically displace strict-constraint reasoning, the reading forecloses its sibling.',
    'If coexistence holds, the kernel-contest structure is legitimate pluralism within jurisprudence. If one forecloses the other, the winning reading has undergone decisive displacement and the losing reading is archaeologically residual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_coexistence, empirical, 'Whether the kernel-contest readings are live alternatives or one has achieved structural dominance.').

omega_variable(
    asymmetric_litigant_access,
    'Do progressive and conservative litigants have genuinely equal access to the pathways for doctrinal reinterpretation, or does the evolutionary reading create structural advantage for reform-oriented claims?',
    'Analysis of appellate success rates by ideological position: if progressive litigants challenging precedent on evolution grounds succeed at higher rates than conservative litigants defending established doctrine, the evolutionary reading confers asymmetric advantage. If rates are equivalent, the reading distributes authority symmetrically.',
    'If asymmetric, the extraction component of the constraint is higher than measured (0.38), and the beneficiary/payer structure is confirmed. If symmetric, the measured extraction accurately reflects the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_litigant_access, empirical, 'Whether the evolutionary framework distributes interpretive authority symmetrically or asymmetrically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t5, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(comm_tr_t5, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(comm_tr_t25, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t5, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 5, 0.26).
narrative_ontology:measurement_basis(comm_be_t5, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(comm_be_t25, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t5, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(comm_su_t5, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 15, 0.2).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(comm_su_t25, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel has three readings instantiated as separate constraints: evolutionary_framework (this story), strict_stare_decisis (backward-binding), and pluralist_balancing (context-dependent). Each reading authors a different ε (referent: the standing arrangement under the reading's own lights), different beneficiary/victim structure, and different measured type. The readings are linked via network.affects_constraints to indicate family membership and indicate that adoption of one reading creates structural pressure on sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
