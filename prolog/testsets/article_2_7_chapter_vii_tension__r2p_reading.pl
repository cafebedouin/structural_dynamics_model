% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Reading of UN Charter Articles 2(7) and Chapter VII
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The R2P reading reinterprets the UN Charter's Article 2(7)
 *   (non-interference in internal affairs) and Chapter VII (security threats)
 *   to make state sovereignty conditional on protecting populations from
 *   systematic atrocity. When a state commits or fails to prevent genocide,
 *   crimes against humanity, ethnic cleansing, or mass sexual violence, the
 *   reading says sovereignty is suspended and international intervention
 *   becomes authorized, even mandatory. This directly contradicts the
 *   sovereignty-first reading, which treats non-interference as foundational
 *   and limits Chapter VII intervention to inter-state aggression. The
 *   constraint described here is ONE reading of a contested kernel: the
 *   Charter text itself. This story instantiates the R2P reading; the sibling
 *   sovereignty_first_reading is a separate constraint story with its own ε,
 *   beneficiaries, and classification. The two readings coexist across
 *   different states, legal traditions, and political coalitions; neither
 *   forecloses the other within a single framework, but they create
 *   structural pressure on each other.
 *
 * KEY AGENTS:
 *   - Persecuted populations: structurally powerless, trapped in territory controlled by atrocity perpetrators; R2P reading declares them the primary beneficiaries of the constraint and the justification for intervention.
 *   - State committing atrocities: institutional power holder experiencing the constraint as delegitimization of its sovereignty claim; the constraint makes atrocity an enforcement trigger rather than an internal matter.
 *   - Intervening state coalition: institutional power, sets intervention terms; gains legitimacy from R2P framing to conduct military/humanitarian action without state consent.
 *   - Security Council P5: gatekeepers of Chapter VII authorization; their veto power means geopolitical interests determine whether R2P is enforced case-by-case.
 *   - Weak state governments (not atrocity perpetrators): face erosion of the non-intervention shield they depend on; their sovereignty becomes conditional on demonstrable protection capacity.
 *   - International legal establishment: provides the doctrinal framework and interpretive authority that makes the R2P reading a live option.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.72).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Reading of UN Charter Articles 2(7) and Chapter VII").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'bc23cce3-4cd2-41e0-bd78-1d5dca236a6d').
narrative_ontology:cs_kernel_codification('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', fixed_text).
narrative_ontology:cs_authority_grounding('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', extraction).
narrative_ontology:cs_interpretation_layer_present('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d').
narrative_ontology:cs_reading_relation('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', sovereignty_conditional_on_protection, deontological).
narrative_ontology:cs_axiom('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', foundational, systematic_atrocity_overrides_non_intervention).
narrative_ontology:cs_axiom_status(systematic_atrocity_overrides_non_intervention, holdable).
narrative_ontology:cs_axiom_grounding('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', systematic_atrocity_overrides_non_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', universal_human_protection_covenant).
narrative_ontology:cs_drift_state('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', contemporary_geopolitical_selectivity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bc23cce3-4cd2-41e0-bd78-1d5dca236a6d', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_norm_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereign_state_capacity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_intervention_principle).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, weak_state_security).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, state_committing_atrocities).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, weak_state_governments).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_intervention_norm_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity — genocide, crimes against humanity, ethnic cleansing, mass sexual violence — from their own state or state-sponsored actors. The R2P reading declares them the primary beneficiaries: international intervention becomes a duty, not a violation of sovereignty, when the state fails or is the perpetrator. Their only exit is physical flight; the constraint legitimizes external rescue.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% Loses the sovereignty shield that non-intervention norms provided. Systematic atrocity becomes grounds for international intervention authorized under R2P reading of Chapter VII. The state experiences the constraint as delegitimization and external interference; its claim to monopoly on legitimate force within its territory is overridden. It pays in sovereignty loss and military/political intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, state_committing_atrocities, payer,
    institutional, generational, constrained, national).

% Gains the legitimacy and framework to intervene militarily, diplomatically, and humanitarily without explicit consent of the targeted state. Sets intervention terms, determines when R2P threshold is met, and controls exit timing. Bears military and political costs but gains strategic access and humanitarian narrative cover. The constraint gives them the authority to act as the international community's enforcement arm.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_state_coalition, agenda_setter,
    institutional, generational, mobile, global).

% Hold the formal authorization gate under Chapter VII. R2P reinterprets their mandate to cover intra-state mass violence. Their geopolitical interests — alliance, resource competition, prestige — become determinants of whether R2P is enforced in any given case. They can veto humanitarian intervention when it conflicts with their interests, making the constraint's application inconsistent and selective.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, security_council_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% Lose the non-intervention shield they depend on for autonomy. Even if not committing atrocities, weak states must now demonstrate protection capacity or risk international intervention justified by R2P. The constraint creates a conditionality they may lack capacity to meet, generating a capacity trap: inability to prevent atrocity triggers intervention that further erodes state capacity and sovereignty.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, weak_state_governments, payer,
    moderate, generational, constrained, national).

% Defend the classical non-intervention principle as foundational to post-1945 order and protection against great-power domination. They experience R2P as erosion of the norm they advocate for. Their advocacy capacity is constrained — the non-intervention principle is already subordinated by the Charter's Chapter VII — but they actively argue R2P overextends humanitarian language to legitimize geopolitical intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_intervention_norm_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, non_intervention_norm_advocates, observer).

% Provides the interpretive authority determining when R2P threshold is met and advising on compliance with humanitarian law during interventions. Occupies the authoritative seat without direct enforcement power; influence flows through Security Council and coalition formation. The body's credibility depends on consistent, objective application of the atrocity threshold, but application has become increasingly selective.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_law_body, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_law_body, observer).

% Are structurally excluded from R2P authorization decisions. Humanitarian intervention can be authorized without their consent, overriding regional sovereignty claims. They would argue for regional autonomy in determining protection standards and intervention triggers, but the R2P reading de-privileges their consent as necessary. Their absence from the gate is part of what R2P accomplishes — it centralizes legitimacy with Security Council and Western powers.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, regional_power_blocs, excluded,
    powerful, generational, mobile, continental).

% Synthesizes and legitimizes the R2P reading through academic scholarship, law school teaching, treaty interpretation, and institutional advice. Does not directly enforce but shapes what counts as acceptable legal interpretation. The establishment's majority has endorsed R2P, marginalizing sovereignty-first voices in institutional legal discourse.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_legal_establishment, observer,
    institutional, generational, analytical, global).

% The populations R2P claims to protect are structurally excluded from decisions about whether intervention occurs, how it is conducted, and when it ends. They lack voice in the authorization process and often carry the collateral costs of military intervention. R2P is authorized and conducted on their behalf without their meaningful participation, treating them as objects of protection rather than agents of their own protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_population_agency_voices, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, intervening_state_coalition).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for authorizing intervention to protect populations from systematic atrocity, solving the collective-action problem of humanitarian crisis response. Without R2P, intervening states face accusations of imperialism or overreach; without coordination, mass atrocities persist without effective response. R2P provides the doctrinal bridge that transforms military action into authorized international duty, enabling coalition formation around the humanitarian mission rather than pure geopolitical interest.
% TRANSFER_FUNCTION: Transfers authority to intervene from the targeted state to an international coalition. The transfer is jurisdiction: the state's claim to monopoly on legitimate force within its territory is overridden by the international community's claim to protect the affected population. Authority flows from the state to intervening powers, legitimized by R2P reading of the Charter.
% ABSENT_VOICES: Weak state governments fear R2P as precedent for external interference, but their objection is marginal to Security Council deliberations. Regional powers excluded from authorization decisions would argue for regional autonomy, but their voices are not systematically heard. Most critically, the persecuted populations themselves are typically absent from decisions about whether intervention occurs, how it is conducted, or when it ends — they are protected subjects, not participants in protection decisions.
% DISAPPEARANCE_RATIONALE: If the R2P reading disappeared overnight, the legitimacy framework for humanitarian intervention would revert to explicit state consent or pure inter-state conflict authorization. Ongoing atrocities would lose the doctrinal cover currently authorizing intervention; weak states would regain presumptive sovereignty over internal affairs; the international system would return to a model where humanitarian response depends on voluntary coalition-building without Charter authorization. The shift in authority allocation is immediate and global — intervention capacity would depend on case-by-case coalition interest rather than doctrine-based legitimacy.
% FOUNDING_PROBLEM: Post-Cold War mass atrocities (Rwanda 1994, Srebrenica 1995, Kosovo 1999) exposed the gap between humanitarian law obligations and enforcement mechanisms. The international community had Genocide Convention obligations but no legitimate path to intervene when states committed atrocities. R2P doctrine was developed to close that gap by reinterpreting the Charter to make population protection a foundational duty, not an optional good.
% FOUNDING_PROBLEM_CORROBORATION: The International Commission on Intervention and State Sovereignty (ICISS 2001) and UN General Assembly (2005 World Summit) formally endorse the founding problem and R2P as solution. However, the International Court of Justice, many Global South governments, and realist scholars contest whether the founding problem was genuine legal ambiguity (the Genocide Convention already obligates states to prevent and punish) or a political gap that R2P exploits for geopolitical purposes. Corroboration is near-universal from human rights and humanitarian advocacy communities; explicitly rejected by many sovereignty-defending states and critical legal scholars. No consensus external corroboration from disinterested parties — the founding problem diagnosis itself is contested.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1995 onward because R2P emerges as a coherent doctrine after Rwanda and Srebrenica. The reading legitimizes intervention that would otherwise violate non-intervention norms, extracting authority from targeted states. The extraction is high (0.68 at interval end) because the constraint conditionalizes sovereignty itself — the most fundamental right a state claims. Suppression is higher (0.72) because R2P depends on actively suppressing the sovereignty-first reading through institutional channels (GA resolutions, Security Council practice, legal scholarship). Theater ratio rises from near-zero to 0.41 because R2P interventions are increasingly accompanied by humanitarian rhetoric that may diverge from actual geopolitical motives — the constraint's deployment is often theatrical cover for power projection. The measurement grid shares one timeline (1945-2024) with every metric authored at every time point, enabling temporal analysis of how the constraint's extractiveness and performance character evolved. Early projections (1945-based) estimate near-zero extractiveness because the R2P reading did not exist; extraction accumulates as the doctrine is synthesized and institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (intervening coalition, P5, legal establishment) and the target seats (atrocity state, weak governments) experience radically different constraints. From the coalition's perspective, R2P is enabler and legitimizer — expansion of the mandate they can invoke. From the weak state's perspective, it is erosion of sovereignty they depend on. From the persecuted population's perspective, it is hope that external rescue is possible (though often unfulfilled). The engine computes per-seat type differently because directionality differs: the coalition sits near beneficiary (d ~ 0.2); the atrocity state sits near full target (d ~ 0.95); weak states sit intermediate (d ~ 0.65). The constraint's single ε (0.68) is applied at each seat with different directionality modifiers to produce different effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are declared beneficiaries: the constraint legitimizes intervention that protects them. However, directionality for persecuted populations sits at d ~ 0.0 (full beneficiary) because they gain access to protection without bearing enforcement costs — intervening states bear those costs. The intervening coalition (agenda-setter) sits at d ~ 0.25 (beneficiary-leaning): they gain legitimacy to intervene without explicit target-state consent, but they also bear military/political risks. The atrocity state sits at d ~ 0.95 (near full target): it loses sovereignty claim and faces intervention. Weak non-atrocity states sit at d ~ 0.70 (toward target): they lose the non-intervention shield even though they are not perpetrators, though their exit options are slightly better (constrained vs. trapped). The non-intervention norm advocates (organized power, constrained exit) sit at d ~ 0.75: they bear advocacy costs without control over the constraint's enforcement. The directionality overrides are unnecessary here because the structural derivation from beneficiary/victim + exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   R2P avoids simple mandatrophy because it retains coordination function alongside extraction. The coordination problem it solves — how to legitimize protection intervention when the state is the perpetrator — is genuine and persistent. However, extractiveness has accumulated over the interval (1945-2024) in ways that suggest mandatrophy risk: (1) P5 geopolitical interests increasingly determine R2P invocation (Libya 2011 yes, Syria 2011-2024 no, despite comparable atrocities), indicating the protection mandate is subordinated to power politics. (2) Theater ratio rises (from 0.25 to 0.41) as humanitarian language covers geopolitical interventions. (3) Weak states' capacity to refuse intervention without being labeled as atrocity-tolerant has eroded, suggesting the constraint's suppression is calcifying. The classification remains tangled_rope (genuine coordination + asymmetric extraction + active enforcement) rather than piton because the coordination function retains real force in cases where atrocity is unambiguous and P5 interests align — but the boundary between coordination and extraction-cover has become increasingly porous. The omega variables address whether the coordination function can be disentangled from the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_cover_story,
    'Does R2P represent genuine coordination on protecting populations from atrocity, or is the coordination frame increasingly cover for great-power military intervention?',
    'Comparative analysis of R2P invocation across atrocity cases: is the doctrine applied consistently regardless of great-power interest (genuine coordination signal) or selectively when aligned with intervening state geopolitical goals (cover-story signal)? Examine Libya (intervened), Syria (not intervened despite comparable atrocities), Myanmar (selective), DRC (sporadic). Post-intervention audit of whether protection improvements persist or military interests dominate.',
    'If predominantly cover-story, R2P reclassifies from tangled_rope (real coordination + extraction) to snare (extraction with humanitarian framing). If genuinely coordinated protection, theater_ratio should decline and per-case consistency should improve. Current theater_ratio rise (0.25 to 0.41) suggests creeping cover-story dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether R2P coordination is sincere or theatrical.').

omega_variable(
    weak_state_capacity_trap,
    'Does R2P''s conditionalization of sovereignty on protection capacity create a structural trap for weak states: inability to prevent atrocity triggers intervention that further erodes state capacity?',
    'Longitudinal study of post-intervention state capacity in R2P cases: do states subject to humanitarian intervention show improved protection capacity and institutional strength, or do they show further degradation (sovereignty erosion, institutional dependency on external actors, loss of monopoly on legitimate force)? Compare intervention cases (Kosovo, Libya, Côte d''Ivoire) against non-intervention cases with similar initial capacity.',
    'If a capacity trap exists, R2P may legitimize a cycle in which weak states are punished (via intervention) for lacking the capacity to meet the constraint''s condition (protection), yet the punishment (intervention) is precisely what prevents capacity-building. This would support reclassification from tangled_rope toward snare and suggest the constraint systematically extracts from weak states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_state_capacity_trap, empirical, 'Whether R2P creates a dependency trap for weak states.').

omega_variable(
    p5_veto_foreclosure,
    'Does P5 veto power over R2P invocation mean the reading forecloses genuine coordination and functions primarily as a tool of geopolitical power?',
    'Analysis of vetoed R2P resolutions (Syria, Myanmar, Venezuela cases): examine whether vetoes cluster by alignment patterns (P5 member protecting ally) or distribute randomly across geopolitical blocs. Test whether veto patterns predict intervention outcomes better than atrocity severity metrics.',
    'If geopolitical alignment predicts veto better than atrocity severity, the R2P reading''s coordination claim is foreclosed by the P5 veto structure — what remains is extraction (legitimizing intervention when P5 interests align) with humanitarian framing. This would support reclassification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_foreclosure, empirical, 'Whether P5 veto power negates R2P''s coordination function.').

omega_variable(
    atrocity_threshold_ambiguity,
    'Is the threshold for ''systematic atrocity'' that triggers R2P clear and objectively determinable, or is it subjectively constructed to rationalize interventions decided on other grounds?',
    'Comparative analysis of atrocity characterization in invoked vs. non-invoked cases: do intervening states consistently frame comparable violence the same way, or do they upgrade severity language in cases they intend to intervene in and downgrade in cases they do not? Examine expert disagreement on whether specific crises met the R2P threshold.',
    'If threshold is genuinely objective and applied consistently, R2P retains coordination legitimacy. If threshold is subjectively deployed post-hoc to rationalize decisions made on other grounds (geopolitical interest), the constraint functions as pure extraction with a humanitarian cover story — snare reclassification candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_ambiguity, empirical, 'Whether the R2P atrocity threshold is objective or post-hoc rationalization.').

omega_variable(
    reading_foreclosure_test,
    'Does the R2P reading logically foreclose the sovereignty-first reading within any single framework, or do they coexist as genuinely incommensurable positions?',
    'Logical analysis: the R2P reading asserts sovereignty is conditional on protection; the sovereignty-first reading asserts sovereignty is foundational and non-intervention is presumptive. These cannot both be true within a single commitment framework IF ''foundational'' and ''conditional'' are contradictory. However, they can coexist if one adopts different frameworks: R2P is a human-rights-first framework; sovereignty-first is a state-autonomy-first framework. Neither forecloses the other unless one framework is shown to be internally incoherent — which framework takes priority?',
    'If the readings genuinely foreclose each other (not coexist), the constraint should be classified as instantiating a resolved conceptual contest, not an open coexistence. The network.affects_constraints edge to sovereignty_first_reading would carry ''forecloses'' rather than ''coexists_with''. This affects how the corpus models intra-kernel dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether R2P and sovereignty-first logically foreclose each other or coexist as incommensurable frameworks.').

omega_variable(
    humanitarian_law_vs_geopolitical_interest_decoupling,
    'Can R2P''s humanitarian mandate be decoupled from P5 geopolitical interest, or is the constraint structurally bound to great-power enforcement capacity?',
    'Thought experiment: if humanitarian law enforcement were delegated to an independent international body (International Court of Justice, International Criminal Court, UN emergency force with autonomous mandate) rather than the Security Council, would R2P interventions increase, decrease, or remain stable? Examine counterfactual cases where P5 interests were not aligned with atrocity severity.',
    'If coordination requires P5 enforcement (as current structure does), R2P cannot escape geopolitical interest filtering — the constraint is structurally extractive. If humanitarian enforcement could be decoupled from P5, R2P''s coordination claim would be stronger. Current structure suggests the coordination function is hostage to power — supporting snare or high-extraction tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_law_vs_geopolitical_interest_decoupling, conceptual, 'Whether R2P''s humanitarian mandate can be institutionally separated from great-power geopolitics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(arti_tr_t1945, projected).
narrative_ontology:measurement(arti_tr_t1995, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(arti_tr_t1995, observed).
narrative_ontology:measurement(arti_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement_basis(arti_tr_t2001, observed).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(arti_tr_t2005, observed).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement_basis(arti_be_t1945, projected).
narrative_ontology:measurement(arti_be_t1995, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(arti_be_t1995, observed).
narrative_ontology:measurement(arti_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement_basis(arti_be_t2001, observed).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(arti_be_t2005, observed).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(arti_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement_basis(arti_su_t1945, projected).
narrative_ontology:measurement(arti_su_t1995, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(arti_su_t1995, observed).
narrative_ontology:measurement(arti_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement_basis(arti_su_t2001, observed).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement_basis(arti_su_t2005, observed).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(arti_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, un_security_council_p5_veto_authority).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, international_humanitarian_law_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'article_2_7_chapter_vii_tension'. The sibling reading 'article_2_7_chapter_vii_tension__sovereignty_first_reading' instantiates the opposite pole: state sovereignty as foundational, intervention narrowly limited to inter-state aggression and explicit consent. The two readings coexist across different state blocs, legal traditions, and geopolitical positions. Both are live options in contemporary international law, but they create ongoing structural pressure on each other. The R2P reading has gained institutional momentum post-Cold War, generating the extraction and suppression dynamics measured here. Neither reading forecloses the other — they represent genuinely incommensurable frameworks (human-rights-first vs. state-autonomy-first) rather than a logical contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
