% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living-Constitution Interpretive Constraint (Enduring Principles, Evolving Application)
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story authors ONE reading of the constitutional-meaning kernel: the
 *   living-constitutionalist arrangement under which federal judges treat the
 *   Constitution's enumerated principles as enduring while treating their
 *   application as legitimately responsive to contemporary moral consensus
 *   and changed circumstances. The standing arrangement under contest — the
 *   referent for epsilon — is this adaptive-interpretation regime as it has
 *   actually operated, assessed by the reading's own lights. It solves a real
 *   coordination problem (governing across time under a rigid amendment rule)
 *   while transferring real decision power from democratic processes to
 *   courts; the metric profile describes that mixed operation independently
 *   of the claim. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: Agenda-setter (institutional/identity_locked) —
 *   administers the adaptive method and receives the transferred interpretive
 *   authority - rights_claimants_in_evolving_contexts: Primary beneficiary
 *   (moderate/constrained) — gains a working channel closed elsewhere -
 *   historically_marginalized_communities: Secondary beneficiary
 *   (organized/constrained) - legal_professional_class: Beneficiary-custodian
 *   (institutional/identity_locked) — reproduces and credentials the method -
 *   elected_branches_and_state_majorities: Primary target
 *   (institutional/constrained) — bears the counter-majoritarian transfer -
 *   voting_majorities_on_overridden_questions: Target (organized/constrained)
 *   - originalist_jurists_and_scholars: Excluded competitor (powerful/mobile)
 *   — method displaced from adjudication -
 *   comparative_constitutional_scholars: Analytical observer — sees the
 *   cross-system structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.47).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living-Constitution Interpretive Constraint (Enduring Principles, Evolving Application)").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '6a727fb7-6e4f-4436-a99b-46df18d5eb93').
narrative_ontology:cs_kernel_codification('6a727fb7-6e4f-4436-a99b-46df18d5eb93', fixed_text).
narrative_ontology:cs_authority_grounding('6a727fb7-6e4f-4436-a99b-46df18d5eb93', lineage).
narrative_ontology:cs_interpretation_layer_present('6a727fb7-6e4f-4436-a99b-46df18d5eb93').
narrative_ontology:cs_reading_relation('6a727fb7-6e4f-4436-a99b-46df18d5eb93', us_constitution_meaning__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6a727fb7-6e4f-4436-a99b-46df18d5eb93', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('6a727fb7-6e4f-4436-a99b-46df18d5eb93', foundational, application_tracks_contemporary_moral_consensus).
narrative_ontology:cs_axiom_status(application_tracks_contemporary_moral_consensus, holdable).
narrative_ontology:cs_axiom_grounding('6a727fb7-6e4f-4436-a99b-46df18d5eb93', application_tracks_contemporary_moral_consensus, instrumental).
narrative_ontology:cs_axiom('6a727fb7-6e4f-4436-a99b-46df18d5eb93', secondary, present_generations_equal_constitutional_authority).
narrative_ontology:cs_axiom_status(present_generations_equal_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('6a727fb7-6e4f-4436-a99b-46df18d5eb93', present_generations_equal_constitutional_authority, deontological).
narrative_ontology:cs_reference_frame('6a727fb7-6e4f-4436-a99b-46df18d5eb93', enduring_principles_adaptive_application).
narrative_ontology:cs_drift_state('6a727fb7-6e4f-4436-a99b-46df18d5eb93', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6a727fb7-6e4f-4436-a99b-46df18d5eb93', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, historically_marginalized_communities).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, legal_professional_class).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, elected_branches_and_state_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, voting_majorities_on_overridden_questions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured federal judges and Supreme Court justices decide which interpretive test governs each case. Under this reading they treat the text's enumerated principles as binding while updating what those principles require in light of current social attitudes, medical knowledge, and institutional practice. Each adaptive ruling extends the bench's authority over questions previously settled elsewhere; judges' opinions, reputations, and clerkship networks are built inside this method, and abandoning it mid-career carries heavy professional cost.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Litigants who invoke broad clauses — equal protection, due process, cruel-and-unusual punishment — to obtain protections the founding generation did not recognize or enforce for people like them. Electoral channels were closed or hostile when they sought relief, so their realistic path runs through federal courts; a reversal of interpretive method leaves them to restart in fifty statehouses.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Communities excluded from full political membership at ratification and for generations afterward — descendants of enslaved people, women before suffrage, unpopular minorities. Adaptive interpretation produced desegregation, recognition of interracial marriage, and heightened equal-protection scrutiny; the community organizations that won these cases maintain litigation infrastructure whose value depends on the method remaining open.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, historically_marginalized_communities, beneficiary,
    organized, generational, constrained, national).

% Congress, presidents, and state legislatures whose statutes and constitutional judgments are invalidated or rewritten when courts update doctrine. Formal escape requires Article V amendment — two-thirds of both houses plus thirty-eight states — rarely assembled under modern conditions; the practical lever is appointment timing, which operates on a scale of decades.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, elected_branches_and_state_majorities, payer,
    institutional, biographical, constrained, national).

% Electoral coalitions that win contested questions at the ballot box — school policy, criminal justice, marriage, abortion regulation — and see the result nullified by a court applying an updated reading. Their recourse is to wait for judicial turnover or to litigate the interpretive question itself.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, voting_majorities_on_overridden_questions, payer,
    organized, biographical, constrained, national).

% Law schools, bar associations, elite firms, and the clerkship economy that teach, staff, and credential the adaptive method. Custody of 'evolving standards' confers interpretive authority and career returns; members police method boundaries through hiring, publication, and nomination advice.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legal_professional_class, beneficiary,
    institutional, generational, identity_locked, national).

% Judges, academics, and advocates committed to fixing constitutional content at ratified public meaning. Inside this reading's operation their method is displaced from adjudication; they contest through scholarship, judicial nominations, and state-level experimentation, and their institutional fortunes have risen sharply in recent appointment cycles.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_jurists_and_scholars, excluded,
    powerful, generational, mobile, national).

% Academic observers comparing how long-lived written constitutions handle the stability-versus-adaptability dilemma across jurisdictions. They take no side in domestic contests and document which arrangements solve the dead-hand problem and at what distributional cost.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a short eighteenth-century text governing a continental, continuously changing society without perpetual amendment: it supplies stable continuity at the level of principles (predictable commitments to equality, liberty, due process) while letting doctrine track transformed circumstances, solving the dead-hand problem that a rigid-amendment constitution otherwise faces.
% TRANSFER_FUNCTION: Moves final decision power over contested social questions — from elected majorities, Congress, and state governments to federal courts — case by case, as judges re-specify what enduring principles require under current conditions.
% ABSENT_VOICES: Originalist jurists and the citizens they represent regard unratified reinterpretation as illegitimate and are outside the adjudicative conversation; their objections register only indirectly through nominations, scholarship, and state-level pushback. Founding-generation voices are absent by definition, which is precisely the locus of the dispute.
% DISAPPEARANCE_RATIONALE: If the adaptive-application constraint vanished overnight and courts bound themselves to fixed founding meaning, doctrines built on evolved application — one-person-one-vote, incorporation of the Bill of Rights against the states, modern equal-protection tiers, evolving-standards Eighth Amendment law — would destabilize across hundreds of precedents; rights claimants would lose their working channel and the elected branches would regain swept-in authority amid severe transition chaos.
% FOUNDING_PROBLEM: The dead-hand problem: how a brief 1787/1791 text with an arduous amendment procedure can govern a vastly transformed society without either obsolescence or perpetual rewriting. The reading crystallized as industrialization, expanded suffrage, and national markets outran the text's founding-era applications.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative constitutional scholarship documents that nearly every long-lived written constitution faces the same stability-adaptability dilemma regardless of its rights politics; amendment-rate histories corroborate that Article V's difficulty is structural, not partisan; and originalist scholars themselves concede the text under-determines many modern questions even as they resolve the under-determination differently.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-substantial (0.47): the regime genuinely moves final decision power on contested questions from electorates to courts, but the transfer is bounded by precedent discipline, confirmation politics, and the fact that courts often ratify consensus formed elsewhere. Suppression (0.38) is enforcement-through-gatekeeping rather than coercion: the method is maintained by professional socialization, hiring and publication norms, and appointment stakes, not by punishing participants — and it suppresses rights expansion far less than a frozen-doctrine regime would, which is the expected structural delta for this reading. Theater ratio (0.28) is low-moderate: invocations of 'enduring principles' sometimes rationalize outcomes reached on policy grounds, but the underlying coordination function is real and load-bearing. Accessibility collapse (0.45): once courts own interpretation, fixed-meaning adjudication is institutionally expensive to reach, yet Article V and appointment politics remain open exits, so alternatives degrade without vanishing. Resistance (0.62) is high and sustained: an organized originalist counter-movement, recurring court-curbing proposals, and deep academic contestation. The judiciary's identity_locked exit is professional-identity fusion: opinions, reputations, and clerkship economies are constituted inside the method, so the classification would shift only if a cohort of judges broke from the frame mid-career. All three tracked series run on one shared six-point grid; the late-interval dip in extractiveness and suppression reflects the originalist turn reclaiming ground (e.g., returning abortion regulation to the political branches), not stabilization of the living regime itself. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the federal_judiciary seat the arrangement is stewardship it performs and benefits from; from elected_branches_and_state_majorities and voting_majorities_on_overridden_questions the same structure operates as unratified rule-making imposed on them with constrained exit; from rights_claimants_in_evolving_contexts it is the only functioning channel they have. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for rights_claimants_in_evolving_contexts, historically_marginalized_communities, and legal_professional_class (the last partially captured into enforcement, nudging its d up from a pure-beneficiary position). Victim declarations drive high directionality for elected_branches_and_state_majorities and voting_majorities_on_overridden_questions; their constrained exit (supermajority amendment bar, decade-scale appointment leverage) pushes them toward the full-target end despite institutional power. The federal_judiciary sits near the beneficiary end as both administrator and recipient of the transferred authority. originalist_jurists_and_scholars are excluded rather than coordinated — their displacement is the boundary the method's enforcement maintains — and sit outside the beneficiary/victim derivation while remaining structurally opposed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing a changing society under a rigid-amendment text — is live, so nothing here approaches mandate atrophy and no sunset applies. The tangled_rope claim guards against symmetric misreadings: a pure-rope verdict would conceal the counter-majoritarian transfer that payer seats demonstrably bear; a pure-snare verdict would erase the genuine dead-hand coordination the regime performs and mispredict its persistence (it survives because the coordination is real, not only because enforcement holds). The classification keeps both components visible and lets the engine price each seat's share.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the us_constitution_meaning kernel; which reading ultimately governs, and does the disagreement reduce to the single control question — ratified public meaning versus contemporary moral consensus as the binding test when they conflict?',
    'Track appointment composition, doctrinal trajectory, and scholarly convergence; the readings'' divergence is located precisely at the control question, so resolution arrives when one test consolidates across the bench and academy or when a hybrid framework stabilizes.',
    'An originalist consolidation raises suppression of rights expansion, shifts the victim set toward rights claimants facing frozen doctrine, and reprices every seat''s directionality; a consolidated living reading entrenches the counter-majoritarian transfer measured here. The positivist sibling changes the validity question, not the interpretive one, and would leave this reading''s internal structure largely intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: kernel membership, reading identity, and the located point of inter-reading disagreement.').

omega_variable(
    adaptive_necessity_vs_judicial_interest,
    'Is adaptive interpretation a genuine coordination necessity for any long-lived rigid-amendment constitution, or a constructed arrangement that primarily serves judicial authority and rights-expansion coalitions?',
    'Comparative analysis across amendment regimes: jurisdictions with easy amendment need less adaptive interpretation; natural experiments where apex courts adopt fixed-meaning methods reveal whether coordination outcomes hold.',
    'If necessity dominates, much of the measured extraction is coordination cost and the classification trends toward rope; if interest-driven, the counter-majoritarian transfer is closer to rent and the classification trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_necessity_vs_judicial_interest, empirical, 'Whether the regime''s coordination function or its beneficiary structure explains its persistence.').

omega_variable(
    counter_majoritarian_transfer_magnitude,
    'How much final decision power actually transfers from electorates to courts under this reading, net of cases where courts merely ratify consensus already formed through politics?',
    'Systematic coding of landmark adaptive rulings against measured opinion timelines: did the Court lead or follow; counterfactual legislative-path analysis for each overridden policy.',
    'Low net transfer supports a rope-leaning reading (courts as lagging indicators of consensus); high net transfer confirms the extraction component and strengthens the payer seats'' effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_transfer_magnitude, empirical, 'Net size of the counter-majoritarian transfer that constitutes this reading''s principal extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t6, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(us_c_be_t6, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(us_c_be_t12, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(us_c_be_t18, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(us_c_be_t24, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(us_c_su_t6, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 6, 0.31).
narrative_ontology:measurement(us_c_su_t12, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(us_c_su_t18, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 18, 0.37).
narrative_ontology:measurement(us_c_su_t24, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'how the Constitution binds.' The kernel us_constitution_meaning splits into three epsilon-invariant readings: originalist (fixed ratified meaning), positivist (enactment-procedure validity), and this living-constitutionalist file (enduring principles, evolving application). Each story carries its own epsilon, beneficiary/victim structure, and classification: this reading's epsilon prices the counter-majoritarian transfer inherent in adaptive application, whereas the originalist sibling locates its costs in frozen-doctrine harms to rights claimants, and the positivist sibling prices procedural-validity enforcement. The upstream/downstream pressure runs through appointment politics and doctrinal citation practice; edges here keep contamination propagation inside the family rather than leaking into unrelated domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
