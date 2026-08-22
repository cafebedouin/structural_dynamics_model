% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Nondelegation Reading of Separation of Powers
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This story authors the formalist reading of the separation-of-powers
 *   kernel: the claim that constitutional text draws a strict, impermeable
 *   line between legislative and executive power, such that Congress cannot
 *   delegate rulemaking authority to agencies beyond narrow ministerial
 *   detail-filling. From 1935 (Schechter Poultry, the last time the Supreme
 *   Court struck a delegation) through the mid-2010s this reading was largely
 *   dormant in practice even where invoked rhetorically; it has been
 *   substantially revived since roughly 2015 through the major questions
 *   doctrine and renewed nondelegation arguments, sharply increasing its
 *   practical bite on agency rulemaking. This is ONE of three constraints in
 *   the separation_of_powers_text kernel family — functionalist_reading and
 *   unitary_executive_reading are separate constraints with their own ε,
 *   beneficiaries, and stakeholders, linked here only via
 *   network.affects_constraints and cs_structure.reading_relations. The ε
 *   authored here is for the formalist reading's own account of the current
 *   arrangement it contests (broad delegated agency authority), assessed by
 *   formalist lights as an unconstitutional usurpation — not for the
 *   strict-nondelegation regime the reading would install.
 *
 * KEY AGENTS:
 *   - congress_as_institution: institutional beneficiary recovering exclusive lawmaking leverage
 *   - regulated_industry_incumbents: organized beneficiary using the doctrine offensively against disfavored rules
 *   - formalist_judiciary: agenda_setter administering the delegation boundary
 *   - federal_administrative_agencies: trapped payer whose statutory authority is destabilized
 *   - environmental_and_safety_beneficiary_public: powerless payer bearing diffuse regulatory-delay costs
 *   - technical_rulemaking_staff: moderate-power payer whose expertise-based role is devalued
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.71).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Nondelegation Reading of Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, 'd56bc697-4cc4-4c8b-af4d-f2be9a2749d1').
narrative_ontology:cs_kernel_codification('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', fixed_text).
narrative_ontology:cs_authority_grounding('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', lineage).
narrative_ontology:cs_interpretation_layer_present('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1').
narrative_ontology:cs_reading_relation('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', foundational, legislative_power_categorically_nondelegable).
narrative_ontology:cs_axiom_status(legislative_power_categorically_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', legislative_power_categorically_nondelegable, deontological).
narrative_ontology:cs_axiom('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', secondary, intelligible_principle_standard_insufficient_safeguard).
narrative_ontology:cs_axiom_status(intelligible_principle_standard_insufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', intelligible_principle_standard_insufficient_safeguard, conventional).
narrative_ontology:cs_reference_frame('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', vesting_clauses_as_exhaustive_categorical_grants).
narrative_ontology:cs_drift_state('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', post_new_deal_administrative_state, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d56bc697-4cc4-4c8b-af4d-f2be9a2749d1', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress_as_institution).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industry_incumbents).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, federal_administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, environmental_and_safety_beneficiary_public).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, technical_rulemaking_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the formalist reading, Congress recovers exclusive claim to legislative power and cannot offload politically costly technical rulemaking to agencies without writing detailed statutory rules itself. Individual members gain leverage: agencies must return to Congress for authority, creating repeated opportunities for oversight, credit-claiming, and negotiation. Congress bears none of the administrative burden of implementation even as it captures the formal authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress_as_institution, beneficiary,
    institutional, generational, arbitrage, national).

% Incumbent firms benefit when agencies cannot issue or update technical rules without new congressional action, since legislative gridlock functions as a de facto moratorium on new regulatory burdens. They can litigate any agency rule as an unconstitutional delegation, using the formalist doctrine as an offensive tool against rules they oppose, and their lobbying resources let them exploit the resulting congressional bottleneck more effectively than diffuse publics can.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industry_incumbents, beneficiary,
    organized, biographical, arbitrage, national).

% Courts adopting this reading administer the boundary: they decide which statutory grants of authority to agencies cross the line into impermissible delegation. This role concentrates significant policymaking power in the judiciary itself, since 'how much specificity is enough' is a judicially drawn line not specified by the constitutional text with the precision the formalist reading claims.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Agencies such as EPA, FCC, and OSHA depend on broad statutory grants (e.g., 'protect public health,' 'in the public interest') to update technical standards as science and technology change. Under strict nondelegation, these grants become constitutionally vulnerable, and agencies cannot exit the constraint — they can only wait for Congress to re-legislate specifics it has neither the technical capacity nor the political will to produce, or narrow their own rules preemptively to avoid litigation risk.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federal_administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Members of the public who rely on agency capacity to respond to emerging hazards (novel pollutants, workplace risks, financial instruments) bear the cost when rulemaking stalls pending congressional re-authorization. They have no direct standing to compel Congress to legislate and no exit from the jurisdiction; the harm is diffuse, delayed, and difficult to trace to the doctrine that caused it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, environmental_and_safety_beneficiary_public, payer,
    powerless, generational, trapped, national).

% Career civil servants with scientific and technical expertise see their work products (rules built on delegated interpretive authority) exposed to constitutional challenge regardless of technical merit. Their functional role — translating broad statutory purposes into operational standards — is precisely what the formalist reading treats as constitutionally suspect, devaluing the expertise-based rationale for their positions.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, technical_rulemaking_staff, payer,
    moderate, biographical, constrained, national).

% Scholars and jurists holding the sibling readings would object that the formalist line-drawing is itself judicially invented rather than textually compelled, and that administrative governance since 1935 rests on a settled functionalist accommodation the formalist reading unsettles. They participate in the broader legal debate but are not part of the coalition currently advancing this reading in courts.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_and_unitary_executive_scholars, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, congress_as_institution).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The formalist reading coordinates a genuine problem: preventing Congress from evading political accountability by handing open-ended lawmaking power to unelected agencies, preserving the constitutional design in which the electorally accountable branch bears responsibility for binding rules.
% TRANSFER_FUNCTION: Moves effective rulemaking authority from technically expert agencies back toward Congress and, functionally, toward the courts that police the delegation boundary; moves the cost of regulatory delay and gridlock onto agencies and the publics who depend on timely technical regulation.
% ABSENT_VOICES: Functionalist and unitary-executive scholars, along with the broader administrative law bar that built doctrine on the post-1935 settlement, would object that this reading discards decades of workable practice for a textual purity the constitutional text does not actually specify with the claimed precision; they are present in scholarship but structurally absent from the formalist coalition's own framing.
% DISAPPEARANCE_RATIONALE: If courts abandoned strict nondelegation tomorrow, most of the modern regulatory state's statutory foundations (broad public-interest grants to EPA, FCC, SEC, OSHA) would return to unquestioned validity, litigation risk against agency rules would collapse, and Congress would lose the leverage it currently gains from forcing agencies back to it for authority — the balance of power between the branches would visibly shift toward the functionalist status quo.
% FOUNDING_PROBLEM: The founding problem is the risk that an elected legislature could evade accountability for unpopular or complex policy choices by delegating open-ended lawmaking power to appointed officials, diffusing responsibility and weakening the constitutional structure of enumerated, separated powers.
% FOUNDING_PROBLEM_CORROBORATION: Formalist jurists and originalist scholars attest the problem is live and worsening as agency rulemaking has expanded. Administrative law scholars, former agency officials, and functionalist judges outside the formalist coalition attest that the accountability problem is largely addressed through modern notice-and-comment procedure, congressional oversight, and judicial review under arbitrary-and-capricious standards — and that the formalist revival responds to ideological opposition to regulatory outcomes rather than a persisting structural defect.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising sharply post-2015 because the formalist reading, once operationalized by courts, transfers substantial regulatory capacity away from agencies without a corresponding transfer of technical capacity to Congress — the coordination function (accountability) is real but the realized effect is asymmetric extraction from agencies and the publics who depend on their rules. Suppression is high (0.78) because the doctrine, once adopted, forecloses agency reliance on broad statutory grants as a class, regardless of the technical merit of any particular rule — this is a structural collapse of an entire category of regulatory tool, not case-by-case scrutiny. Theater ratio is comparatively low (0.28): the doctrinal machinery does real work reshaping outcomes; it is not primarily performative. Accessibility collapse (0.62) reflects that once a court adopts strict nondelegation, agencies have no adjacent workaround — they cannot simply rewrite the rule under different statutory language, since the constitutional defect attaches to the underlying delegation itself.
 *
 * PERSPECTIVAL GAP:
 *   From Congress and formalist judges, the arrangement looks like restoration of proper constitutional order — coordination in service of accountability. From agencies and the publics dependent on their rules, the identical doctrinal structure operates as extraction: authority they exercised for decades is withdrawn, with no replacement mechanism capable of producing equivalent technical regulation at the pace modern hazards require. The engine computes these as distinct seat-level classifications from the same structural data; the divergence is real, not an authoring error.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and industry incumbents sit near the beneficiary end: Congress gains leverage without administrative burden, and incumbents gain both a shield against new regulation and a sword against existing rules, with resources to exploit the resulting bottleneck. Agencies sit near the full-target end: they are trapped (cannot exit the jurisdiction or the doctrine) and structurally weakened by design. The diffuse public sits near the target end but with lower salience per capita, consistent with powerless/trapped exit options. The judiciary is coded as agenda_setter rather than a straightforward beneficiary, since it administers rather than collects the transferred authority, though its interpretive discretion is itself a form of gained power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative accountability evasion via delegation) is contested as still live: formalist advocates treat modern agency rulemaking as evidence the problem never went away, while functionalist critics point to notice-and-comment procedure and arbitrary-and-capricious review as functional accountability substitutes developed since 1946 (APA) that the formalist reading does not credit. This is precisely the mandatrophy question the classification is built to surface: is the doctrine solving a live constitutional problem, or is it a revived instrument whose real function is now to block substantively disfavored regulation under a formally neutral banner? The high and rising suppression/extraction trend after 2015 is consistent with the latter reading but does not settle it definitively — hence the omega below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_reading_genuine_vs_instrumental,
    'Is the formalist nondelegation revival a genuine return to constitutional first principles, or an instrumentally selected doctrine deployed primarily to block substantively disfavored regulatory outcomes (environmental, labor, financial) regardless of delegation structure?',
    'Track whether formalist nondelegation arguments are invoked symmetrically against delegations producing politically favored versus disfavored outcomes; asymmetric invocation patterns (only against disfavored regulation) would support the instrumental account.',
    'If instrumental, the coordination story (accountability) is largely cover and the constraint is better classified nearer snare than tangled_rope; if genuine and symmetric, the coordination function is more substantial relative to the extraction, supporting the tangled_rope classification as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_reading_genuine_vs_instrumental, empirical, 'Whether the formalist revival is principled or outcome-selective in application.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the constitutional text itself compel the formalist reading over the functionalist reading, or is the choice between readings underdetermined by text and driven by extratextual judicial and political commitments?',
    'Comparative analysis of founding-era administrative practice (customs collection, land office adjudication) which involved considerable delegated discretion even in the earliest Congresses; if such practice is inconsistent with strict nondelegation, the formalist reading''s claim to textual/originalist compulsion weakens.',
    'If underdetermined, this reading''s high accessibility_collapse (0.62) may overstate how completely the formalist account forecloses functionalist alternatives — the collapse is a judicial choice, not a logical necessity, and could be reversed by future courts without any change in the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the formalist/functionalist split is textually compelled or judicially constructed.').

omega_variable(
    judicial_beneficiary_self_dealing,
    'Does the formalist_judiciary''s role as agenda_setter constitute a form of self-interested power accumulation, since the doctrine transfers effective policymaking discretion to courts deciding delegation-boundary cases?',
    'Examine whether formalist judges'' post-nomination rulings expand judicial discretion over delegation boundaries relative to pre-nomination doctrinal predictions; a consistent pattern of self-expanding discretion would support treating the judiciary as a partial beneficiary rather than a neutral agenda_setter.',
    'If judicial self-dealing is substantiated, formalist_judiciary should be reclassified from agenda_setter to a dual agenda_setter/beneficiary role, which would further concentrate the extraction and strengthen the case against pure coordination framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_beneficiary_self_dealing, conceptual, 'Whether courts administering the doctrine are also beneficiaries of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 1935, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__formalist_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(sepa_tr_t1970, separation_of_powers_text__formalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(sepa_tr_t1995, separation_of_powers_text__formalist_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(sepa_tr_t2015, separation_of_powers_text__formalist_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(sepa_tr_t2022, separation_of_powers_text__formalist_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(sepa_tr_t2026, separation_of_powers_text__formalist_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__formalist_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(sepa_be_t1970, separation_of_powers_text__formalist_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(sepa_be_t1995, separation_of_powers_text__formalist_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(sepa_be_t2015, separation_of_powers_text__formalist_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(sepa_be_t2022, separation_of_powers_text__formalist_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(sepa_be_t2026, separation_of_powers_text__formalist_reading, base_extractiveness, 2026, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__formalist_reading, suppression_requirement, 1935, 0.2).
narrative_ontology:measurement(sepa_su_t1970, separation_of_powers_text__formalist_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(sepa_su_t1995, separation_of_powers_text__formalist_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(sepa_su_t2015, separation_of_powers_text__formalist_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(sepa_su_t2022, separation_of_powers_text__formalist_reading, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement(sepa_su_t2026, separation_of_powers_text__formalist_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__formalist_reading, 0.1).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the separation_of_powers_text kernel. formalist_reading (this story) and functionalist_reading share the same constitutional text but author sharply different ε, beneficiary/victim sets, and classifications: functionalist_reading treats broad delegation as legitimate coordination with agencies as beneficiaries of a workable regulatory settlement, while this story treats the same delegation as the object of extraction, with agencies as victims of doctrinal overreach. unitary_executive_reading addresses a related but distinct axis (intra-executive unity, e.g. removal power over independent agency heads) rather than inter-branch delegation, and is linked here because both readings draw on the same formalist interpretive methodology and jointly pressure the functionalist settlement from different directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
