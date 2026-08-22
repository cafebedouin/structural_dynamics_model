% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity — International Oversight Reading
 *   domain: International Law / Criminal Justice / State Sovereignty
 *
 * SUMMARY:
 *   This story instantiates the international-oversight reading of the
 *   Article 17 complementarity kernel: complementarity as an accountability
 *   trigger, where the ICC functions as guardian against impunity whenever
 *   domestic proceedings are judged non-genuine. Under this reading,
 *   'unwilling or unable' is interpreted broadly enough to capture not only
 *   total judicial collapse but also victor's justice, sham trials, and
 *   elite-protective proceedings dressed as prosecution. The referent for
 *   extractiveness is the standing complementarity arrangement as this
 *   reading sees it operating today — an arrangement that reaches sham
 *   domestic proceedings and captures officials who believed a domestic
 *   process (however hollow) had discharged their exposure. The sibling
 *   national-primacy reading, where national courts are presumptively
 *   adequate absent proof of sham, is a separate constraint with its own ε
 *   and its own victim set — it is not blended into this file.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office
 *   - victims_in_complicit_states
 *   - victims_in_failed_states
 *   - sham_prosecution_defendants
 *   - targeted_state_officials
 *   - non_signatory_state_nationals
 *   - domestic_judiciaries_of_implicated_states
 *   - un_security_council_permanent_members
 *   - international_law_scholars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.42).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.38).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity — International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "International Law / Criminal Justice / State Sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '86366291-4128-421a-886a-aa1219a48599').
narrative_ontology:cs_kernel_codification('86366291-4128-421a-886a-aa1219a48599', formalized).
narrative_ontology:cs_authority_grounding('86366291-4128-421a-886a-aa1219a48599', practice).
narrative_ontology:cs_interpretation_layer_present('86366291-4128-421a-886a-aa1219a48599').
narrative_ontology:cs_reading_relation('86366291-4128-421a-886a-aa1219a48599', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('86366291-4128-421a-886a-aa1219a48599', foundational, unwillingness_encompasses_captured_judiciaries).
narrative_ontology:cs_axiom_status(unwillingness_encompasses_captured_judiciaries, holdable).
narrative_ontology:cs_axiom_grounding('86366291-4128-421a-886a-aa1219a48599', unwillingness_encompasses_captured_judiciaries, conventional).
narrative_ontology:cs_axiom('86366291-4128-421a-886a-aa1219a48599', secondary, genuine_proceeding_requires_independence_not_merely_form).
narrative_ontology:cs_axiom_status(genuine_proceeding_requires_independence_not_merely_form, holdable).
narrative_ontology:cs_axiom_grounding('86366291-4128-421a-886a-aa1219a48599', genuine_proceeding_requires_independence_not_merely_form, instrumental).
narrative_ontology:cs_reference_frame('86366291-4128-421a-886a-aa1219a48599', rome_statute_negotiated_compromise).
narrative_ontology:cs_drift_state('86366291-4128-421a-886a-aa1219a48599', post_al_bashir_cooperation_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86366291-4128-421a-886a-aa1219a48599', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, icc_prosecutorial_office).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, sham_prosecution_defendants).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, targeted_state_officials).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, non_signatory_state_nationals).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, no_impunity_for_atrocity_crimes).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, genuine_national_proceeding_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assesses whether domestic proceedings are 'genuine' under Article 17(2)-(3), interpreting 'unwilling or unable' broadly to include cases of sham prosecution, shielding of suspects, unjustified delay, or lack of independence. Initiates admissibility challenges and requests state cooperation. Its authority and continued relevance depend on being seen as willing and able to act where domestic systems fail or protect elites.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Survivors and families of atrocity crimes committed or tolerated by their own government, where domestic courts are staffed, financed, or intimidated by the same power structure implicated in the crimes. Under this reading, they gain an external venue for justice that their own state will not provide; without ICC intervention their claims disappear into non-prosecution or symbolic proceedings.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, national).

% Populations in states where judicial infrastructure has collapsed entirely — no functioning courts, no prosecutorial capacity, no security to gather evidence. This reading treats their access to ICC jurisdiction as the only route to any accountability at all, since 'inability' captures collapsed as well as captured judiciaries.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_failed_states, beneficiary,
    powerless, biographical, trapped, national).

% Individuals who underwent domestic proceedings — sometimes convictions with lenient sentences, sometimes acquittals engineered by allied prosecutors — that the ICC subsequently deems inadequate to shield them from double jeopardy under Article 20(3). Under the broad reading, they face renewed international prosecution despite having already been through a national process, because that process is read as designed to protect rather than punish them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, sham_prosecution_defendants, payer,
    moderate, biographical, constrained, national).

% Heads of state, military commanders, and senior officials in states whose domestic legal systems are read as captured by the same political-military apparatus under investigation. They experience the broad 'unwilling or unable' standard as removing the sovereign shield they expect domestic jurisdiction to provide, particularly where their own state's courts are unlikely ever to independently investigate them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, targeted_state_officials, payer,
    powerful, biographical, constrained, national).

% Nationals of states that never ratified the Rome Statute but face ICC jurisdiction through UN Security Council referral or territorial jurisdiction over conduct on a member state's soil. Under the broad reading, their state's non-membership does not shield them if the referral or territorial hook exists and their national proceedings (if any) are judged non-genuine.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, non_signatory_state_nationals, payer,
    powerful, biographical, constrained, global).

% National courts and prosecutors whose independence or genuineness is under direct challenge by the ICC's admissibility determination. They rarely have a formal voice in shaping how 'unwilling or unable' is interpreted at the treaty level, even though the standard is applied directly to their institutional legitimacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, domestic_judiciaries_of_implicated_states, excluded,
    institutional, generational, trapped, national).

% Three of five permanent members are non-signatories who can refer situations to the ICC via Security Council resolution while insulating their own nationals from equivalent scrutiny. Their asymmetric position — able to trigger the mechanism against others while remaining structurally shielded — is central to charges of victor's justice, yet they are not parties bound by the same admissibility exposure.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, un_security_council_permanent_members, excluded,
    institutional, generational, arbitrage, global).

% Assess whether the broad reading of complementarity coheres with the Rome Statute's negotiating history and whether its application has, in practice, tracked genuine impunity gaps or instead tracked geopolitical power — disproportionately reaching weak and non-aligned states while sparing powerful non-signatories.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, diffuse).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a backstop accountability mechanism so that atrocity crimes do not go permanently unpunished merely because the state with primary jurisdiction is unwilling to prosecute its own officials or unable to function as a judiciary at all — solving the collective-action problem of impunity where domestic incentives point toward shielding perpetrators.
% TRANSFER_FUNCTION: Moves prosecutorial authority and the coercive apparatus of international law from the domestic state to the ICC whenever domestic proceedings are judged non-genuine; moves exposure to prosecution from previously-shielded state officials and elites onto the ICC's docket; moves symbolic and practical vindication toward victims in states that would not otherwise pursue their case.
% ABSENT_VOICES: Domestic judiciaries whose competence and independence are being adjudicated have no formal seat in the admissibility determination that passes judgment on them. Non-signatory powerful states that can trigger referrals against others face no reciprocal exposure, and their absence from the treaty regime is precisely what makes the broad reading read, to some observers, as selectively enforced.
% DISAPPEARANCE_RATIONALE: If the broad 'unwilling or unable' standard were replaced by a narrow, deferential standard (or the complementarity trigger removed entirely), a substantial set of currently-reachable cases — sham prosecutions, delayed or symbolic proceedings, judiciaries captured by implicated regimes — would fall outside ICC reach permanently. Victims in complicit and failed states would lose their only forum; officials currently exposed to renewed prosecution despite domestic proceedings would gain durable protection.
% FOUNDING_PROBLEM: The Rome Statute's drafters needed a jurisdictional trigger that would let the ICC act as a court of last resort without displacing national sovereignty wholesale — the founding problem was closing the impunity gap created by states unwilling to prosecute their own, or too collapsed to prosecute anyone, without becoming a general-jurisdiction supranational court.
% FOUNDING_PROBLEM_CORROBORATION: Victim advocacy organizations, UN human rights bodies, and independent international law scholars outside the ICC's own institutional interest attest that domestic shielding of implicated officials remains widespread and that sham or delayed proceedings continue to occur in multiple situation-countries; this corroboration comes from parties who do not benefit institutionally from an expansive ICC caseload, distinguishing it from the ICC's own self-assessment of its necessity.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).
:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the core coordination function — closing an impunity gap when states genuinely fail — is authored as real, not merely a cover story, so this is not authored as a pure snare. But the broad admissibility standard does impose real costs on defendants who underwent (even if flawed) domestic proceedings, and on officials whose states' judiciaries are judged non-genuine by an external body they cannot appeal to on equal footing. Suppression is lower than extraction (0.38) because the mechanism relies on treaty cooperation and Security Council referral rather than direct coercive enforcement — the ICC has no independent police power and depends on state parties for arrest and surrender, which caps how much suppressive force the constraint alone can apply. Resistance is comparatively high (0.62) reflecting the sustained pushback from powerful non-signatory states and from targeted officials who dispute the genuineness determinations made against their domestic systems.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of victims in complicit or failed states, the broad reading looks like Rope or a genuine Tangled Rope skewed toward coordination — an accountability backstop finally reaching them. From the seat of targeted officials and sham-prosecution defendants, the identical admissibility standard looks like Snare-adjacent extraction: a body applying an elastic, externally-defined 'genuineness' test that can override a domestic process they regarded as final. The engine computing per-seat classifications from directionality and exit options is expected to diverge sharply between these two positions even though both are reacting to the same Article 17(2)-(3) text as read under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims in complicit/failed states are declared beneficiaries because the broad reading structurally exists to give them a forum; their exit options are trapped (no domestic alternative), which under the derivation chain would normally push d toward the target end, but because they are declared beneficiaries of THIS mechanism specifically (not targets of it), the derivation correctly reads them as low-d recipients of the coordination benefit despite their powerlessness. Targeted officials and sham-prosecution defendants are victims of the mechanism's reach — moderate to powerful agents whose exit options are constrained (they cannot simply leave the jurisdiction of international law once a referral attaches), pushing their derived d toward the target end. Non-signatory state nationals are a distinct victim category because their exposure arises from territorial or Security Council hooks rather than treaty consent, which is a structurally different route to the same target-end directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as live (impunity gaps persist, corroborated by sources outside the ICC's own institutional interest), which blocks a mandatrophy read of pure institutional self-perpetuation. But the tension the story holds open is that the SAME structural evidence (broad interpretation reaching sham proceedings) that vindicates the mechanism's continued necessity from one seat is exactly what critics read as scope creep from another. The classification as tangled_rope rather than snare or rope reflects that both dynamics are authored as simultaneously present and real, not that one is illusory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_standard_indeterminacy,
    'Where is the line between a domestic proceeding that is merely imperfect (and therefore still ''genuine'') and one that is a sham (and therefore triggers ICC admissibility)? The Rome Statute text does not specify a bright-line test.',
    'Track ICC Pre-Trial and Appeals Chamber jurisprudence over successive admissibility rulings (e.g., Al-Senussi, Gaddafi, Kenyatta) to see whether a stable doctrinal standard has crystallized or whether determinations remain ad hoc and outcome-driven.',
    'A crystallized, predictable standard would support reading the mechanism as principled coordination (rope-leaning); persistent ad hoc, outcome-driven determinations would support reading the broad interpretation as a discretionary extraction lever exercised selectively against weaker states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_standard_indeterminacy, empirical, 'Whether ''unwilling or unable'' has a stable doctrinal content or functions as an elastic, case-by-case discretionary standard.').

omega_variable(
    selective_enforcement_geopolitics,
    'Does the broad reading''s practical application track genuine impunity gaps, or does it disproportionately reach weaker, non-aligned, or African states while sparing powerful non-signatories whose nationals commit comparable conduct?',
    'Comparative empirical analysis of ICC situation-country selection against base rates of qualifying atrocity allegations globally, controlling for referral mechanism (self-referral, UNSC referral, proprio motu).',
    'If selection tracks power rather than gravity of conduct, the broad reading''s coordination function is substantially compromised by an extraction pattern — the mechanism would function partly as victor''s justice in exactly the direction its proponents claim it corrects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_geopolitics, empirical, 'Whether case selection under the broad reading correlates with state power rather than conduct severity.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice between the international_oversight_reading and the national_primacy_reading itself a matter that international law resolves determinately, or is it genuinely open — meaning the Rome Statute''s complementarity provision is irreducibly ambiguous between two coherent institutional visions?',
    'Examine the Rome Statute''s travaux préparatoires and the negotiating history at the 1998 Rome Conference for evidence of which reading the drafting states intended, versus evidence that the ambiguity was a deliberate diplomatic compromise allowing both readings to coexist in the ratified text.',
    'If travaux préparatoires show deliberate ambiguity-as-compromise, both readings are equally legitimate constructions of the same kernel and their coexistence is a feature, not a defect, of the treaty. If one reading is shown to be the drafters'' clear intent, the other reading''s authority claim weakens considerably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the kernel''s dual-reading structure reflects genuine drafting ambiguity or a determinate original meaning obscured by later contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__international_oversight_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(arti_tr_t21, article_17_complementarity__international_oversight_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement(arti_tr_t27, article_17_complementarity__international_oversight_reading, theater_ratio, 27, 0.28).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__international_oversight_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(arti_be_t21, article_17_complementarity__international_oversight_reading, base_extractiveness, 21, 0.4).
narrative_ontology:measurement(arti_be_t27, article_17_complementarity__international_oversight_reading, base_extractiveness, 27, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__international_oversight_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(arti_su_t21, article_17_complementarity__international_oversight_reading, suppression_requirement, 21, 0.36).
narrative_ontology:measurement(arti_su_t27, article_17_complementarity__international_oversight_reading, suppression_requirement, 27, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, un_security_council_referral_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, state_party_cooperation_obligations).

% DUAL FORMULATION NOTE:
% This constraint and article_17_complementarity__national_primacy_reading are two readings of the single article_17_complementarity kernel, per the ε-invariance decomposition principle. This reading authors a lower admissibility threshold (broader ICC reach, higher authored extractiveness against targeted officials and sham-prosecution defendants); the sibling reading authors a higher admissibility threshold (narrower ICC reach, lower extractiveness, victim set limited to proven-sham cases). Each carries its own stable ε; they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
