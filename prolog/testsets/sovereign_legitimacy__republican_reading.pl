% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Legitimacy: Upward Delegation of Authority through Popular Sovereignty
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The republican reading of sovereign legitimacy asserts that political
 *   authority derives from the consent of the governed, flows upward through
 *   delegated mechanisms (elections, constitutional procedures), and is
 *   accountable downward through electoral removal and constitutional limits.
 *   This constraint embodies the modern theory of popular sovereignty: the
 *   people are the ultimate source of authority; governors hold power in
 *   trust, revocable through democratic processes. The reading competes with
 *   monarchical legitimacy (authority flows downward from inherited right and
 *   divine sanction) and constitutional-hybrid readings (authority is
 *   dual-sourced: ceremonial inherited authority and political delegated
 *   authority, mediated by constitutional law). This story instantiates ONLY
 *   the republican reading; sibling readings are separate constraint stories
 *   linked via network edges.
 *
 * KEY AGENTS:
 *   - Enfranchised citizens: the notional sovereign source; periodically delegate through elections; benefit from accountability promise; constrained by limited participation and information asymmetries.
 *   - Elected representatives and officials: hold delegated authority; accumulate discretion between elections; publicly accountable, practically insulated.
 *   - Disenfranchised and excluded populations: bear governance costs without franchise; structurally excluded by the framework's definition of 'the people'; trapped within the polity.
 *   - Constitutional courts: interpret the framework; claim neutrality but shape what popular sovereignty means in practice.
 *   - Non-citizens and non-residents: subject to governance without consent mechanisms; excluded from the delegation process.
 *   - Competing sovereigns (other polities, international actors): subordinated by the monopoly-legitimacy claim; structurally barred from the consent process.
 *   - Theorists and observers: document slippage between the framework and practice; propose alternative readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Legitimacy: Upward Delegation of Authority through Popular Sovereignty").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '7d78233f-3c95-46af-b4f4-920e883ab61b').
narrative_ontology:cs_kernel_codification('7d78233f-3c95-46af-b4f4-920e883ab61b', formalized).
narrative_ontology:cs_authority_grounding('7d78233f-3c95-46af-b4f4-920e883ab61b', lineage).
narrative_ontology:cs_interpretation_layer_present('7d78233f-3c95-46af-b4f4-920e883ab61b').
narrative_ontology:cs_reading_relation('7d78233f-3c95-46af-b4f4-920e883ab61b', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d78233f-3c95-46af-b4f4-920e883ab61b', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('7d78233f-3c95-46af-b4f4-920e883ab61b', foundational, authority_source_is_people_will).
narrative_ontology:cs_axiom_status(authority_source_is_people_will, holdable).
narrative_ontology:cs_axiom_grounding('7d78233f-3c95-46af-b4f4-920e883ab61b', authority_source_is_people_will, deontological).
narrative_ontology:cs_axiom('7d78233f-3c95-46af-b4f4-920e883ab61b', foundational, legitimacy_requires_ongoing_validation).
narrative_ontology:cs_axiom_status(legitimacy_requires_ongoing_validation, holdable).
narrative_ontology:cs_axiom_grounding('7d78233f-3c95-46af-b4f4-920e883ab61b', legitimacy_requires_ongoing_validation, empirically_contingent).
narrative_ontology:cs_axiom('7d78233f-3c95-46af-b4f4-920e883ab61b', secondary, accountability_through_removal_mechanisms).
narrative_ontology:cs_axiom_status(accountability_through_removal_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('7d78233f-3c95-46af-b4f4-920e883ab61b', accountability_through_removal_mechanisms, instrumental).
narrative_ontology:cs_reference_frame('7d78233f-3c95-46af-b4f4-920e883ab61b', delegated_authority_validated_through_elections).
narrative_ontology:cs_drift_state('7d78233f-3c95-46af-b4f4-920e883ab61b', contemporary_with_incumbent_insulation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7d78233f-3c95-46af-b4f4-920e883ab61b', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizens).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, democratic_institutions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_non_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizens_subject_to_governance).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The notional source of all political authority under the republican reading. They periodically delegate power through elections, recall mechanisms, and constitutional amendment. They benefit from the framework's promise of accountability and responsive governance, but their actual power varies with organization, information access, and participation rates. Exit would mean abandoning citizenship or the polity itself — structurally trapped.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizens, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizens, beneficiary).

% Hold delegated authority from citizens between elections. The republican framework treats them as agents of the people, accountable through electoral removal and constitutional limits. In practice, they accumulate discretion and often resist downward accountability. They benefit from the legitimacy the popular-sovereignty frame confers while often operating beyond the scope voters could directly observe or control.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    powerful, biographical, mobile, national).

% Historically excluded from the franchise (enslaved people, women, non-property-owners, non-citizens). They bear the costs of governance decisions made without their consent while having no mechanism to register dissent or remove decision-makers. The republican framework validates their exclusion by redefining 'the people' to exclude them. They cannot exit without abandoning the polity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_populations, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, disenfranchised_populations, excluded).

% Interpret and enforce the constitutional limits on delegated authority. They claim to be neutral arbiters of the framework, not power-holders themselves, but their interpretation choices shape what the people's sovereignty actually means in practice. They sit between elected authority and citizen appeal.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Immigrants, residents, and others governed by the polity but without franchise rights. They are subject to laws they have no voice in making or removing. The republican framework excludes them from the consent mechanism while claiming the legitimacy to govern them. Exit requires immigration or internal relocation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_citizens_subject_to_governance, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, non_citizens_subject_to_governance, excluded).

% Other polities and their representatives whose authority is subordinated by the republican reading's assertion that THIS polity's people are the ultimate sovereign. They would contest the monopoly on legitimacy claim but are structurally barred from the franchise and consent mechanisms.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, competing_sovereigns_and_external_powers, excluded,
    institutional, generational, trapped, global).

% Intellectuals and analysts who study the constraint. They document whether the republican reading's claims about delegation, accountability, and consent match the actual operation of the polity. They can propose alternative readings (monarchical, hybrid) and provide evidence of drift between the framework and practice.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_theorists_and_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decision-making procedure that binds all residents to rules produced through a process claiming to represent the people's collective will. Replaces hereditary or divine legitimation with a consensual framework: those governed have (notionally) authorized the governors through elections and constitutional acceptance. Coordinates millions of independent agents into a lawful order without requiring external force to establish the initial consent.
% TRANSFER_FUNCTION: Transfers authority from the mass of citizens (the people) to a small number of elected representatives and officials between elections. Also transfers the burden of state capacity-building, tax collection, and enforcement to citizens in the form of compliance obligations and resources. Moves power from the many to the few, justified by the claim that the few hold it in trust and on delegation.
% ABSENT_VOICES: The historically disenfranchised — enslaved people during the founding era, women until the 19th and 20th centuries, non-property owners excluded by censuses, indigenous peoples, non-citizens — would have contested the framework's claim that 'the people' includes them or authorized their exclusion. They are kept out by the same legal and social mechanisms the framework uses to define legitimacy. Contemporary migrants and non-citizens subject to the polity's laws but without franchise would object if heard.
% DISAPPEARANCE_RATIONALE: If the republican legitimacy framework vanished overnight — no elections, no constitutional limits, no appeal to popular sovereignty — the polity would either collapse into civil conflict (as citizens reassert claims to authority) or reorganize around a different legitimacy source (hereditary rule, military order, technocratic expertise, theocracy). The entire architecture of modern electoral democracy depends on this constraint's persistence.
% FOUNDING_PROBLEM: How can multiple independent persons be bound by rules without a natural sovereign (king, divine agent, tribal elder) to impose them? The founding problem is the coordination puzzle: establish authority legitimate enough that millions comply without constant external coercion, and accountable enough that it does not calcify into unchecked tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Theorists across traditions acknowledge the coordination problem the republican reading addresses: Hobbes, Locke, Rousseau, and contemporary democratic theorists all treat the need to bind a multitude to rules as real. However, they dispute whether the republican solution (popular sovereignty through elections) actually solves it or merely disguises other forms of power. Constitutional historians document that the founding generations explicitly invoked the framework to justify excluding women, enslaved people, and non-property owners — the coordination was never truly inclusive. Political scientists find recurring slippage between the framework's promises (accountability through elections) and practice (representatives accumulating discretion, minorities suppressed by majorities, money influencing agenda).
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the republican reading does establish genuine coordination benefit (binding millions to rules without constant external force) and periodic validation mechanisms (elections). However, extraction persists because: (1) representatives accumulate discretion between elections, (2) the franchise is never fully inclusive (disenfranchised populations bear costs without consent), (3) structural inequalities in participation (wealth, education, mobilization capacity) mean some citizens exercise far more authority over outcomes than others, (4) the framework legitimates exclusions by redefining 'the people' to exclude inconvenient groups. Suppression is moderate (0.38 at interval end) because the framework permits dissent and mobilization (free speech, assembly), but active suppression mechanisms exist for excluded populations and for challenging the legitimacy of elections themselves. Theater ratio rises slightly mid-interval (peaks at 0.30 around t=30) then moderates, reflecting periods when electoral performance becomes more theatrical (competitive politics, campaign spectacle dominating substance) followed by renewed engagement with substantive governance. Accessibility collapse is moderate-high (0.72): once citizens understand the republican framework, alternatives (hereditary rule, dictatorship, exit from the polity) seem nearly impossible — but exit options remain theoretically available (emigration, secession), preventing complete collapse. Resistance is moderate (0.55): the framework generates continuous internal resistance (parties contest elections, interest groups lobby, minorities protest exclusion, theorists dispute the reading) but this resistance is partly absorbed into the framework itself (elections, amendment procedures, courts) which dampens the resistance trajectory. All metrics are authored on one shared time grid spanning 40 units.
 *
 * PERSPECTIVAL GAP:
 *   The enfranchised citizen seat should compute the constraint as rope (genuine coordination with periodic validation). The disenfranchised/excluded seat should compute closer to snare (governance without consent, active suppression of alternative arrangements, identity-locked exit). The representative seat oscillates: approaching rope when electoral accountability is salient, approaching snare when they operate beyond electoral oversight. The constitutional court seat computes as neutral arbiter but functionally shapes the framework — a subtle snare signature if courts use constitutional interpretation to entrench power beyond democratic revision. The international observer (theorist) seat computes as pure analysis — they see the framework's gap between promise and practice but are not subject to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizens and democratic institutions are declared beneficiaries: they collect legitimacy from the framework (their authority is validated, their participation is treated as foundational) and coordination benefits (rules are accepted as binding without constant enforcement). However, their effective extraction is dampened (low to negative χ) because they notionally control the framework through elections and amendments. Disenfranchised populations and non-citizens are declared victims: they bear governance costs (laws apply to them, resources are extracted for state capacity) without being permitted to withdraw consent or remove decision-makers. Their directionality is high (near 1.0, full target status) because they are trapped within the polity and identity-locked to the status of non-citizen or disenfranchised — exit means abandoning community, family, and home. Representatives and officials sit near symmetric (d ≈ 0.5) in structure: they benefit from delegated authority but are (notionally) accountable through removal. In practice, they accumulate advantages (campaign funding, information asymmetries, institutional insulation) that shift their effective d toward beneficiary territory, but the framework's removal mechanisms prevent complete transition to snare-side extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The republican reading's mandate is to establish legitimate authority through upward delegation and periodic validation. This mandate remains LIVE: elections, constitutional amendment procedures, and removal mechanisms persist and are actively used. The framework has not become a hollow shell maintained for theatrical reasons. However, theater ratio shows a gradual rise from 0.15 to 0.30 between t=0 and t=25, suggesting that electoral competition increasingly operates as spectacle (campaign performance, personality politics) rather than substantive policy validation. This partial mandatrophy is consistent with political scientists' finding that citizens increasingly experience elections as theater while participation in substantive governance (committee work, local organizing, constitutional deliberation) declines. The framework is not piton (atrophied function maintained theatrically) but it shows creeping mandatrophy: the delegation mechanism persists and is formally used, but its ability to actually validate or remove authority erodes as theater rises. Classified as ROPE, not SCAFFOLD or PITON, because the founding problem remains contested (some view the framework as successfully solving coordination; others view it as always having been inadequate to include the disenfranchised) and no sunset clause exists — the constraint is treated as permanent, not transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_scope_paradox,
    'Who exactly counts as ''the people'' whose consent legitimates authority? The framework asserts popular sovereignty but historically excludes slaves, women, non-property owners, non-citizens, and sometimes other categories. Is the exclusion a defect in the implementation of the framework, or is the framework inherently dependent on a restricted definition of ''the people''?',
    'Historical and comparative analysis: (1) did founders explicitly intend exclusions or did they regard them as temporary? (2) do contemporary polities with maximally inclusive franchise show structurally different legitimacy dynamics than those with restricted franchise? (3) can the framework coherently extend franchise indefinitely or does expansion require abandoning the republican reading itself?',
    'If exclusions are implementation defects, the framework is salvageable by expanding franchise — legitimacy is regained through inclusion. If the framework requires a restricted ''people'' to function as claimed, then universal-franchise polities are not truly republican but some hybrid (oligarchic republic, delegated theocracy, or other). The classification might shift from rope (coordination + periodic validation) toward tangled_rope (coordination for the included, extraction from the excluded) or snare (governance without consent disguised as popular will).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchise_scope_paradox, conceptual, 'Whether republican legitimacy is compatible with universal franchise.').

omega_variable(
    accountability_gap,
    'How much discretion can representatives accumulate between elections before the delegation relationship breaks? The framework treats elections as the validation mechanism, but if representatives develop structural insulation (campaign funding dependence, gerrymandering, institutional memory that outlasts electoral cycles, information asymmetries), does the accountability loop still function?',
    'Comparative case studies of polities with identical republican frameworks but different actual patterns of accountability: legislatures with high turnover vs. low turnover; strong party discipline vs. weak; campaign finance rules varying; constitutional amendment rates varying. Measure whether variation in these factors predicts divergence between electoral outcomes and policy change.',
    'High discretion accumulation (structural insulation) would mean the constraint operates closer to snare (governance without meaningful consent, removal mechanisms non-functional) than rope. Low discretion (genuine electoral accountability) maintains the rope classification. Moderate discretion (some insulation, some accountability) holds the rope classification but with elevated theater ratio — the framework functions but increasingly performs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_gap, empirical, 'Whether the accountability mechanism actually constrains representative discretion or has become theater.').

omega_variable(
    majoritarian_tyranny_suppression,
    'The republican framework is vulnerable to majoritarian tyranny: majorities voting to suppress minorities, or to exclude groups from the franchise. How much active suppression is required to keep this danger in check, and does the suppression itself become extractive from the perspective of minorities?',
    'Empirical audit: measure suppression mechanisms actively deployed to prevent majoritarian exclusion (constitutional courts striking down laws, judicial review, minority protections, supermajority requirements, federal division of power). Compare polities with strong such mechanisms to those without; measure whether minorities'' actual exit options and participation rates differ.',
    'If substantial suppression is required, the constraint shifts closer to tangled_rope (coordination for all, but enforced to prevent majority defection) or even snare (minorities suppressed to maintain the system''s stability). If minimal suppression suffices, the constraint remains rope. High suppression with low effectiveness (minorities feel suppressed but exit/removal options remain) maintains rope but with elevated theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_suppression, empirical, 'Whether suppression mechanisms are necessary for stability and whether they become extractive.').

omega_variable(
    reading_contrast__monarchical_foreclosure,
    'Does the republican reading''s core premise — that authority legitimately flows upward from the people — logically rule out the monarchical reading''s core premise that authority legitimately flows downward from inherited right? Or can they coexist as alternative legitimacy sources in the same polity?',
    'Conceptual analysis: in a polity with a ceremonial monarch and elected parliament, are the two readings truly foreclosed from each other, or are they describing different (non-overlapping) authority streams (ceremonial/symbolic vs. political)? Can a single person be both a legitimate heir (monarchical) and a democratically elected representative (republican)? Historical: did early constitutional monarchies resolve this or did they paper over the contradiction?',
    'If the readings foreclose each other, they cannot coexist in a single framework — a polity must choose republican OR monarchical. If they coexist, the classification shifts from competitive (sibling coexists_with) to hybrid (reading_relations = influences or coexists_with, depending on how authority is divided). This affects whether the constitutional-hybrid reading is a genuine third alternative or a unstable compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contrast__monarchical_foreclosure, conceptual, 'Foreclosure relationship between republican and monarchical readings of legitimacy.').

omega_variable(
    extraction_internalization,
    'Do disenfranchised and excluded populations accept the republican framework''s legitimacy even while being excluded from it? If so, is their suppression (acceptance of exclusion) structural (legal barriers preventing participation) or internalized (they have come to believe they do not belong to ''the people'')?',
    'Post-enfranchisement trajectory: when excluded groups gain franchise rights, does suppression persist (suggesting internalization)? Do they immediately exercise the franchise or do participation rates take generations to normalize (suggesting identity-fusion with excluded status)? Do excluded groups reject the framework''s legitimacy outright or claim they are being excluded from a legitimate system they accept?',
    'If suppression is structural (legal barriers), removing the barriers should immediately reduce extraction χ. If suppression is internalized (identity fusion, learned helplessness), removing the barriers may not reduce extraction — the excluded carry suppression with them. This affects whether the classification should shift toward snare (effective suppression via internalization) or whether it remains rope (structural barriers removable through legal change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_internalization, empirical, 'Whether exclusion-acceptance is structural or internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t5, sovereign_legitimacy__republican_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(sove_tr_t5, observed).
narrative_ontology:measurement(sove_tr_t10, sovereign_legitimacy__republican_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(sove_tr_t10, observed).
narrative_ontology:measurement(sove_tr_t15, sovereign_legitimacy__republican_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(sove_tr_t15, observed).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__republican_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(sove_tr_t20, observed).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__republican_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(sove_tr_t25, observed).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__republican_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(sove_tr_t30, observed).
narrative_ontology:measurement(sove_tr_t35, sovereign_legitimacy__republican_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement_basis(sove_tr_t35, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(sove_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t5, sovereign_legitimacy__republican_reading, base_extractiveness, 5, 0.37).
narrative_ontology:measurement_basis(sove_be_t5, observed).
narrative_ontology:measurement(sove_be_t10, sovereign_legitimacy__republican_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(sove_be_t10, observed).
narrative_ontology:measurement(sove_be_t15, sovereign_legitimacy__republican_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(sove_be_t15, observed).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__republican_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(sove_be_t20, observed).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__republican_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement_basis(sove_be_t25, observed).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__republican_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(sove_be_t30, observed).
narrative_ontology:measurement(sove_be_t35, sovereign_legitimacy__republican_reading, base_extractiveness, 35, 0.41).
narrative_ontology:measurement_basis(sove_be_t35, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(sove_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t5, sovereign_legitimacy__republican_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(sove_su_t5, observed).
narrative_ontology:measurement(sove_su_t10, sovereign_legitimacy__republican_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(sove_su_t10, observed).
narrative_ontology:measurement(sove_su_t15, sovereign_legitimacy__republican_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(sove_su_t15, observed).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__republican_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(sove_su_t20, observed).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__republican_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(sove_su_t25, observed).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__republican_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(sove_su_t30, observed).
narrative_ontology:measurement(sove_su_t35, sovereign_legitimacy__republican_reading, suppression_requirement, 35, 0.37).
narrative_ontology:measurement_basis(sove_su_t35, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(sove_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The constraint `sovereign_legitimacy` is a contested kernel instantiated by three structurally distinct readings. This story (`sovereign_legitimacy__republican_reading`) declares beneficiaries as enfranchised citizens and victims as the disenfranchised/excluded, with moderate extractiveness due to the franchise being the validation mechanism. The monarchical reading (`sovereign_legitimacy__monarchical_reading`) declares beneficiaries as the hereditary sovereign and their institutional continuity, with victims as those whose dissent to the inherited authority cannot be expressed through removal mechanisms. The constitutional-hybrid reading (`sovereign_legitimacy__constitutional_hybrid_reading`) splits authority: ceremonial inherited (beneficiary: symbolic continuity; victim: those who reject inherited symbolism) and political delegated (beneficiary: enfranchised; victim: disenfranchised). The three stories share a kernel (the legitimate source of authority) but diverge in what that source IS, who benefits from treating it as legitimate, and what mechanisms validate or fail to validate it. ε-invariance is preserved: each story has a single, stable extraction value derived from its own beneficiary/victim structure and persistence mechanisms. The readings are linked here via affects_constraints to enable the committer-frame analysis (which reading is foreclosing or coexisting with which).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, powerless, 0.88).
constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
