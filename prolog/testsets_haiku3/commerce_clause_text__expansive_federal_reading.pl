% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Clause Reading
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The expansive federal reading of the Commerce Clause interprets
 *   'interstate commerce' to encompass all economic activity with substantial
 *   aggregate effects on national markets. Under this reading, a farmer's
 *   wheat crop affects commodity prices (Wickard v. Filburn, 1942); marijuana
 *   grown for personal use affects the interstate drug market (Gonzales v.
 *   Raich, 2005); discrimination in a local restaurant affects interstate
 *   commerce in lodging and travel (Heart of Atlanta Motel v. United States,
 *   1964). The reading transformed the constitutional foundation of federal
 *   regulatory authority: it enables Congress to regulate intrastate
 *   activity, environmental protection, labor standards, and consumer safety
 *   under the Commerce Clause. This constraint is ONE reading of the
 *   contested commerce_clause_text kernel. The sibling readings —
 *   originalist_narrow_reading (commerce limited to goods crossing state
 *   borders) and substantial_effects_limited_reading (federal power requires
 *   nexus to truly interstate activity) — hold different interpretations of
 *   the same constitutional text. This story instantiates the expansive
 *   reading and models its structural effects: it benefits federal regulatory
 *   authority and national policy coherence advocates, and it subordinates
 *   state autonomy and local economic variation. The measurement series
 *   tracks the reading's entrenchment and theatrical dimension from 1937
 *   (post-Lochner constitutional revolution) through 2024.
 *
 * KEY AGENTS:
 *   - federal_administrative_state (institutional, agenda-setter): sets the scope of federal jurisdiction and enforces the expansive reading through regulatory statutes and executive action
 *   - national_regulatory_coherence_advocates (organized, beneficiary): environmental, labor, and consumer organizations that benefit from uniform federal standards
 *   - state_governments (powerful, payer): bear subordination of state regulatory authority; constrained exit via constitutional amendment
 *   - supreme_court_majority (institutional, agenda-setter): interprets and validates the reading through case law; their interpretive authority is the enforcement mechanism
 *   - originalist_constitutional_schools (organized, observer/excluded): articulate the alternative reading, produce scholarship, litigate challenges; excluded from the authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.42).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Clause Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '64867d14-d4e8-49de-b959-21f0c46674ad').
narrative_ontology:cs_kernel_codification('64867d14-d4e8-49de-b959-21f0c46674ad', fixed_text).
narrative_ontology:cs_authority_grounding('64867d14-d4e8-49de-b959-21f0c46674ad', lineage).
narrative_ontology:cs_interpretation_layer_present('64867d14-d4e8-49de-b959-21f0c46674ad').
narrative_ontology:cs_reading_relation('64867d14-d4e8-49de-b959-21f0c46674ad', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('64867d14-d4e8-49de-b959-21f0c46674ad', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('64867d14-d4e8-49de-b959-21f0c46674ad', foundational, interstate_commerce_includes_substantial_effects).
narrative_ontology:cs_axiom_status(interstate_commerce_includes_substantial_effects, holdable).
narrative_ontology:cs_axiom_grounding('64867d14-d4e8-49de-b959-21f0c46674ad', interstate_commerce_includes_substantial_effects, empirically_contingent).
narrative_ontology:cs_axiom('64867d14-d4e8-49de-b959-21f0c46674ad', foundational, federal_regulatory_authority_extends_to_intrastate_activity).
narrative_ontology:cs_axiom_status(federal_regulatory_authority_extends_to_intrastate_activity, holdable).
narrative_ontology:cs_axiom_grounding('64867d14-d4e8-49de-b959-21f0c46674ad', federal_regulatory_authority_extends_to_intrastate_activity, deontological).
narrative_ontology:cs_reference_frame('64867d14-d4e8-49de-b959-21f0c46674ad', federal_jurisdiction_over_national_economic_effects).
narrative_ontology:cs_drift_state('64867d14-d4e8-49de-b959-21f0c46674ad', contemporary_regulatory_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64867d14-d4e8-49de-b959-21f0c46674ad', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_regulatory_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_variation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal regulatory agencies (EPA, FTC, OSHA, DOL, etc.) set economic and social policy under the reading that any activity with substantial aggregate effects on national markets falls under federal jurisdiction. This reading vastly expands the domain of federal regulatory authority — a farm's output affects commodity prices nationally, a workplace practice affects labor markets nationally, local pollution affects interstate commerce in goods and services. The administrative state collects authority through this reading and defends it against state sovereignty challenges. The reading's persistence requires continuous judicial validation of the 'substantial effects' test.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Environmental advocates, labor advocates, consumer protection organizations, and national business coalitions benefit from uniform federal standards. They argue that state-by-state variation creates a race to the bottom: states compete for business by lowering environmental or labor standards, and only federal authority can prevent this. They benefit from the reading because it enables federal statutes (Clean Air Act, Civil Rights Act, Fair Labor Standards Act) to preempt state law and establish national floors. They perceive a genuine coordination problem solved: the alternative is conflicting state regimes that leave gaps for bad actors.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_regulatory_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% States lose regulatory authority over economic activity within their borders. Under the expansive reading, they cannot set different labor standards, environmental rules, or commercial regulations without federal preemption. They bear the cost of subordination: lost revenue from regulation-dependent economic activity, diminished capacity to respond to local conditions, and the political cost of appearing unable to protect local interests. Their exit option is formal constitutional amendment — a trapped modality. They mount resistance through federalism litigation, but the structural subordination persists regardless of case outcomes because the reading itself is anchored in judicial precedent (Wickard v. Filburn, Gonzales v. Raich) that has become entrenched.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    powerful, generational, constrained, national).

% The abstract capacity for local economies to develop idiosyncratic regulations and norms suited to local conditions is suppressed. Communities cannot experiment with local labor standards, zoning frameworks tied to local ecology, or commercial rules adapted to local trade patterns without federal override. This is a structural loss rather than an actor's loss, but it bears real costs: reduced capacity for policy experimentation, diminished adaptive capacity when national rules misfit local conditions, and suppression of alternative regulatory models that might become evidence for national policy reform.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_variation, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__expansive_federal_reading, local_economic_variation).

% Jurists and legal scholars who believe in a limited reading of federal commerce power (originalists, federalism traditionalists) are structurally excluded from the framework this reading instantiates. They argue the expansive reading misreads the text, ignores original public meaning, and enables unlimited federal jurisdiction. Their objections are lodged in dissenting opinions and law review articles but are not seated in the authority structure that validates the reading — they cannot change the interpretive frame once it is institutionalized in Supreme Court precedent and administrative practice. They experience this as the reading having foreclosed their preferred interpretation through accumulated institutional weight.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, conservative_federalism_jurists, excluded,
    institutional, generational, constrained, national).

% The Supreme Court majority that endorses the expansive reading (Wickard era through contemporary doctrine) interprets the Commerce Clause text and sets the boundaries of permissible federal authority. They enforce the reading through decisions that uphold federal statutes as within constitutional authority. Their enforcement mechanism is sustained by the legitimacy accorded to the Court's interpretive authority. As long as a majority of justices hold this reading, they can rejectfederalism challenges without judicial reclassification of federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Academic and judiciary constituencies (Federalist Society, originalist scholars, some lower-court judges) observe and contest the expansive reading. They articulate the alternative reading (originalist_narrow_reading), produce scholarship supporting it, and litigate cases that invite the Court to adopt it. They have not succeeded in shifting the Court majority but remain an active, articulate dissenting constituency with institutional platforms.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_constitutional_schools, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without uniform federal authority over economic activity with interstate effects, states compete by lowering standards, and gaps emerge that allow races to the bottom in environmental protection, labor rights, and consumer safety. A single actor (the federal government) interpreting the Commerce Clause to encompass all activity with substantial aggregate national effects establishes uniform floors and prevents state-by-state fragmentation.
% TRANSFER_FUNCTION: Transfers regulatory authority from state and local governments to the federal administrative state, and from local economic variation to national policy coherence. Communities lose the right to set different economic rules; states lose the authority to regulate commerce within their boundaries; and in exchange, the nation receives uniform environmental, labor, and commercial standards that apply everywhere. The transfer is not of money but of decision-making power.
% ABSENT_VOICES: Originalist jurists and conservative federalism advocates who hold the alternative reading (originalist_narrow_reading) are excluded from the authority structure that validates this reading. They dissent but are not seated in the Supreme Court majority or the administrative agencies that enforce the expansive interpretation. Communities that would prefer local regulatory variation are not represented as agents because local jurisdiction itself is the suppressed alternative.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight — i.e., if the Supreme Court adopted the originalist_narrow_reading and struck down federal statutes based on commerce-power overreach — the federal administrative state would lose jurisdiction over vast domains. The EPA, OSHA, FTC enforcement, and federal labor regulation would shrink. States would reassume authority over local economic activity. Uniform national standards would fragment into fifty different regimes. The world would rearrange dramatically: environmental regulation would vary by state, labor law would balkanize, and markets currently governed by federal rules would reorganize around state boundaries.
% FOUNDING_PROBLEM: In the early 20th century, the Supreme Court struck down federal economic regulation as exceeding the Commerce Clause — most notably the National Industrial Recovery Act (Lochner era, pre-1937). Congress could not regulate purely intrastate activity even if it had large aggregate effects on interstate commerce. States competed by lowering labor and environmental standards to attract business, creating races to the bottom. The founding problem was: how can the federal government establish national economic standards when the Constitution appears to limit its authority to interstate trade, while most harmful activity (pollution, workplace abuse, cartel conduct) occurs intrastate?
% FOUNDING_PROBLEM_CORROBORATION: Progressives and labor advocates corroborate the founding problem: they cite Lochner-era voids where federal regulation could not address national harms; they point to state-by-state variation in labor and environmental standards as evidence of the race-to-the-bottom mechanism. However, originalist jurists and federalism scholars dispute the problem: they argue the Framers understood intrastate activity as outside federal authority by design, not as a void; they argue competitive federalism (the ability of states to experiment) is a feature, not a bug. The corroboration is partial — aligned with the reading's own beneficiaries — and contested by an organized, intellectually coherent alternative constituency.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68 at interval end): The reading transfers regulatory authority from state governments to the federal administrative state. States are the structural targets — they lose decision-making power over economic activity within their borders. The federal administrative state is the structural beneficiary — it collects authority to regulate vast domains previously thought outside federal jurisdiction. The reading is NOT pure coordination because the transfer is asymmetric: the federal government gains concentrated authority while states lose distributed authority. Extractiveness rises over the interval (0.35 in 1937 to 0.68 in 2024) because the reading has become more entrenched — it is no longer contested in the executive and legislative branches, only in conservative jurisprudence and federalism scholarship. SUPPRESSION (0.42 at interval end): The reading does not require high suppression for persistence because it is validated by the Supreme Court and incorporated into the structure of federal statutes. The suppression that does exist operates against federalism challenges: states that litigate to recover authority are suppressed by courts applying the expansive reading. The suppression metric is relatively low because the reading is institutionalized and widely accepted in the political mainstream — it does not require coercive enforcement beyond normal judicial review. Suppression rises modestly over the interval (0.28 in 1937 to 0.42 in 2024) as the alternative reading becomes less viable in mainstream jurisprudence and more confined to dissenting voices. THEATER (0.28 at interval end): The constraint shows a moderate and rising theater ratio because federal justifications for commerce-based regulation have become increasingly attenuated from the real coordination function. Early justifications (preventing state races to the bottom in labor standards, establishing national environmental floors) map closely to real problems. But by the contemporary era, federal regulation based on 'substantial effects' on interstate commerce can reach highly attenuated intrastate activity (Raich's home-grown marijuana; Sebelius's Affordable Care Act mandate on whether to purchase insurance). The theater ratio rises because the stated basis (substantial interstate effects) is increasingly pretextual — the real basis is that Congress wants to regulate the activity and uses the Commerce Clause as the constitutional peg. The federal administrative state continues to invoke the substantial-effects test, but the connection between the stated basis and the real motivation has become theatrical.
 *
 * PERSPECTIVAL GAP:
 *   Federal administrative agencies (agenda-setter seat) experience this constraint as legitimate federal authority to address genuine national problems — they see themselves as solving races to the bottom and establishing national standards. The Supreme Court majority that endorses the reading sees itself as interpreting the Constitution to permit Congress to address modern national economic problems. State governments (payer seat) experience the same constraint as unconstitutional subordination and usurpation of reserved powers. Conservative federalism jurists see the reading as a departure from the text and the Framers' intent. From the federal seat, the reading is a justified expansion to match constitutional text to modern economic reality. From the state seat, it is an extraction of authority that was constitutionally reserved to the states. The claim/metric gap is intentional: the reading is CLAIMED as tangled_rope (genuine coordination function + asymmetric extraction), and the authored metrics reflect that structure — high extractiveness because of the authority transfer, moderate suppression because it is institutionalized, and rising theater because the stated basis has become attenuated from real problems.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal administrative state holds d ≈ 0.1 (full beneficiary): it collects authority, does not lose authority, and has arbitrage-grade exit (the reading is internally reinforcing — the more authority the federal government exercises, the more precedent accumulates for the expansive reading). State governments hold d ≈ 0.85 (near full target): they are constrained to constrained exit (litigating federalism challenges is their only recourse, and it has rarely succeeded since 1937), they lose authority, and they are geographically trapped (they cannot leave the federal system). National regulatory coherence advocates hold d ≈ 0.25 (modest beneficiary): they benefit from uniform federal standards, they have mobile exit (they can organize in other venues like state legislatures if federal regulation fails), and they have moderate power (they can mobilize political coalitions). The asymmetry between beneficiary (federal state, high power, arbitrage exit) and victim (state governments, high power, trapped exit) is the core extractiveness signature: the constraint persists because the federal government has power to enforce it and states lack power to escape it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state races to the bottom in labor and environmental standards) is LIVE in the 1937-1980 period (core era of New Deal and Great Society expansion). By the contemporary era (2000-2024), the founding problem has PARTIALLY DIED: labor standards are largely set by federal statute, environmental protection is primarily federal, and states are not competing by lowering standards. However, the reading persists and has EXTENDED beyond the original problem — it now supports federal jurisdiction over marijuana, health insurance mandates, and other intrastate activity with only attenuated interstate effects. This is the mandatrophy signature: the constraint persists because it has become institutionalized and because the federal administrative state benefits from it, not because the founding coordination problem is actively live. The theater ratio rise (from 0.08 to 0.28) is the diagnostic for this drift — the reading's stated basis (substantial effects on interstate commerce) is increasingly pretextual; the real function is federal regulatory authority. The constraint qualifies as mandatrophy-resolved: a legitimacy claim (the Commerce Clause permits federal regulation of activity with substantial aggregate effects) has outlived its foundational justification (preventing state races to the bottom) and persists through institutional inertia and beneficiary entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_effects_attenuation,
    'What is the threshold for ''substantial aggregate effects on national markets''? As federal commerce power has expanded, the threshold for what counts as ''substantial'' has become increasingly attenuated — can a home-grown marijuana plant count because it affects the interstate drug market? Is the threshold objective and stable, or does it expand and contract with political preferences?',
    'Systematic analysis of Supreme Court decisions defining ''substantial effects'' from Wickard (1942) through contemporary cases (Sebelius, Gonzales). Measure the proportion of intrastate activity the Court accepts as satisfying the test over time. Compare the stated basis (effects on interstate commerce) with the real basis (congressional desire to regulate) through legislative history and agency testimony.',
    'If the threshold has become so attenuated that nearly any intrastate activity qualifies, the reading has transformed from a coordination mechanism (preventing races to the bottom) into pure extractive authority (federal jurisdiction over anything the government wants to regulate). If the threshold is stable, the reading retains a principled limiting function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantial_effects_attenuation, empirical, 'Whether the ''substantial effects'' standard has become too attenuated to limit federal jurisdiction or remains a meaningful threshold.').

omega_variable(
    reading_vs_text_fit,
    'Does the text of the Commerce Clause — ''Congress shall have power to regulate interstate commerce'' — actually permit the expansive reading, or does the reading require textual interpretation that departs from the original public meaning?',
    'Historical analysis of how the phrase ''interstate commerce'' was understood in 1787-1789 and how it was applied in early federal cases (Gibbons v. Ogden, 1824). Compare the original understanding with the contemporary expansive reading. Assess whether the reading represents a defensible interpretation of the text or a constitutional amendment in substance without formal amendment.',
    'If the expansive reading is a legitimate interpretation of the text as written, the constraint is a reading of the kernel that is textually grounded. If it requires textual departure, the reading is a de facto constitutional amendment that uses the Commerce Clause as a vehicle for expanded federal power. This would support the originalist alternative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_text_fit, conceptual, 'Whether the expansive reading is a defensible interpretation of the constitutional text or a textual departure.').

omega_variable(
    federalism_benefits_vs_costs,
    'What are the actual benefits of uniform federal standards (prevented races to the bottom, economies of scale, national coordination) versus the actual costs (reduced state experimentation, diminished adaptive capacity, suppressed local variation)? Has the constraint produced net benefit or net loss?',
    'Comparative institutional analysis of regulatory outcomes under federal versus state/local control (environmental quality, labor standards, consumer protection). Natural experiments from jurisdictions with greater state autonomy or from periods with less federal preemption. Analysis of regulatory innovation: has federal dominance increased or decreased the rate of policy experimentation and evidence-based reform?',
    'If the constraint produces net benefit (lower pollution, better labor conditions, stronger consumer protection), it is genuine tangled_rope: real coordination function plus asymmetric extraction. If it produces net loss (stagnation, regulatory capture by federal interests, suppression of useful local variation), it is closer to snare: the coordination story is cover for federal extraction of state authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_benefits_vs_costs, preference, 'Whether the expansive reading has produced net social benefit or net cost relative to federal alternatives.').

omega_variable(
    alternative_readings_coexistence,
    'Can the originalist_narrow_reading and the substantial_effects_limited_reading coexist with the expansive reading within a single constitutional framework, or does the expansive reading''s institutional entrenchment foreclose the alternatives?',
    'Assessment of whether a new Supreme Court majority could adopt the originalist or limited reading without formally overruling precedent (through narrow holdings, distinction-making, or limiting principles). Evaluation of whether the alternatives have theoretical coherence independent of the expansive reading.',
    'If the alternatives remain theoretically live (coexist_with relation), the kernel contest is open and the readings are competing interpretations. If the expansive reading has become so institutionalized that the alternatives are foreclosed in practice (forecloses relation), the contest is functionally closed even if it remains nominally open.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_coexistence, conceptual, 'Whether alternative readings remain live options or have been institutionally foreclosed by the expansive reading''s entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1960, commerce_clause_text__expansive_federal_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement_basis(comm_tr_t1960, observed).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_text__expansive_federal_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1980, observed).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__expansive_federal_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(comm_tr_t2000, observed).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__expansive_federal_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement_basis(comm_tr_t2012, observed).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__expansive_federal_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement_basis(comm_be_t1960, observed).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(comm_be_t1980, observed).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(comm_be_t2000, observed).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement_basis(comm_be_t2012, observed).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.28).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement_basis(comm_su_t1960, observed).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement_basis(comm_su_t1980, observed).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement_basis(comm_su_t2000, observed).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement_basis(comm_su_t2012, observed).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, federal_preemption_doctrine__dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, state_police_power_doctrine__reserved_powers).

% DUAL FORMULATION NOTE:
% The commerce_clause_text kernel decomposes into three readings with distinct ε values, stakeholder structures, and institutional effects. The expansive_federal_reading enables vast federal jurisdiction (high ε, federal beneficiary, state victim). The originalist_narrow_reading restricts federal jurisdiction to interstate trade (low ε, state beneficiary, federal victim). The substantial_effects_limited_reading permits federal regulation with limiting principles (moderate ε, symmetric distribution). Each reading constitutes a different constraint because the referee (what counts as valid federal authority) differs. They are linked via network.affects_constraints because the adoption of one reading affects the others' institutional viability — the expansive reading currently constrains the others as minority positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
