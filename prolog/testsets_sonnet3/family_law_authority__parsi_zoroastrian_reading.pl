% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Endogamous Marriage Governance (Family Law Authority Kernel)
 *   domain: religious_governance/family_law
 *
 * SUMMARY:
 *   This story instantiates the parsi_zoroastrian_reading of the
 *   family_law_authority kernel: marriage governed as a
 *   community-preservation institution under Zoroastrian priestly authority,
 *   with endogamy as the central mechanism. Unlike the sacramental readings
 *   (Hindu, Christian) or contractual readings (Muslim, secular), this
 *   reading's distinguishing structural feature is that marriage validity is
 *   bound to communal survival logic under demographic scarcity —
 *   intermarriage does not merely change ritual status, it triggers loss of
 *   communal membership and trust eligibility, with a documented gender
 *   asymmetry (matrilineal descent penalized more than patrilineal). The
 *   coordination function (preserving a genuinely small, historically
 *   persecuted community's distinct identity) is real; the extraction
 *   (disproportionate cost borne by women and children who did not choose the
 *   marriage) is also real and runs through the same structure — hence
 *   tangled_rope rather than a clean rope or pure snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.58).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Endogamous Marriage Governance (Family Law Authority Kernel)").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '3febb535-46ce-44fe-8590-272b5f9f8709').
narrative_ontology:cs_kernel_codification('3febb535-46ce-44fe-8590-272b5f9f8709', distributed).
narrative_ontology:cs_authority_grounding('3febb535-46ce-44fe-8590-272b5f9f8709', lineage).
narrative_ontology:cs_interpretation_layer_present('3febb535-46ce-44fe-8590-272b5f9f8709').
narrative_ontology:cs_reading_relation('3febb535-46ce-44fe-8590-272b5f9f8709', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('3febb535-46ce-44fe-8590-272b5f9f8709', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3febb535-46ce-44fe-8590-272b5f9f8709', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('3febb535-46ce-44fe-8590-272b5f9f8709', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('3febb535-46ce-44fe-8590-272b5f9f8709', foundational, endogamy_as_precondition_for_community_membership).
narrative_ontology:cs_axiom_status(endogamy_as_precondition_for_community_membership, holdable).
narrative_ontology:cs_axiom_grounding('3febb535-46ce-44fe-8590-272b5f9f8709', endogamy_as_precondition_for_community_membership, conventional).
narrative_ontology:cs_axiom('3febb535-46ce-44fe-8590-272b5f9f8709', secondary, patrilineal_descent_determines_child_communal_eligibility).
narrative_ontology:cs_axiom_status(patrilineal_descent_determines_child_communal_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('3febb535-46ce-44fe-8590-272b5f9f8709', patrilineal_descent_determines_child_communal_eligibility, conventional).
narrative_ontology:cs_reference_frame('3febb535-46ce-44fe-8590-272b5f9f8709', post_persecution_diaspora_endogamy_norm).
narrative_ontology:cs_drift_state('3febb535-46ce-44fe-8590-272b5f9f8709', contemporary_demographic_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3febb535-46ce-44fe-8590-272b5f9f8709', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_institutions).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_couples).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsis).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_mixed_marriages).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the navjote (initiation) and ritual marriage ceremony required for recognized Parsi status and marriage validity. Determines who may enter fire temples and receive religious rites. Controls the interpretive tradition on who counts as Parsi, and has historically ruled that children of Parsi women marrying non-Parsis are not admissible to the faith while children of Parsi men marrying out sometimes are — a asymmetry the priesthood itself enforces and defends as doctrinal.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).

% Trusts, panchayats, and endowed funds (housing, charitable, educational) administer resources reserved for recognized community members. Endogamy enforcement protects the boundary that determines eligibility for these resources. Institutions benefit from a stable, bounded membership pool whose scarcity increases the value of continued affiliation.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_community_institutions, agenda_setter).

% Marry within the community, receive full ritual recognition, retain access to communal housing trusts, charitable funds, and fire temple access for themselves and their children. Their marriage is uncontested and their community status secure.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_couples, beneficiary,
    moderate, biographical, constrained, national).

% Marry outside the community and lose access to fire temple rites, communal housing trust eligibility, and in some panchayat rulings, formal community membership itself. They face a binary choice between marrying whom they choose and retaining institutional standing built over generations; there is no partial or negotiated middle path in most jurisdictions' panchayat practice.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsis, payer,
    moderate, biographical, constrained, national).

% Bear a documented asymmetric cost relative to Parsi men who marry out: historically and in many panchayat interpretations, children of Parsi women who marry non-Parsi men are denied navjote and community membership, while children of Parsi men marrying out are more often admitted. This gendered asymmetry compounds the endogamy cost specifically onto women, who cannot avoid it by any individual action once they marry out.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out, payer,
    powerless, biographical, trapped, national).

% Have no say in their parents' marriage choice but inherit its consequences: contested or denied eligibility for navjote initiation, fire temple access, and communal trust benefits, with outcomes varying by jurisdiction and panchayat ruling and by which parent was Parsi.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_mixed_marriages, payer,
    powerless, biographical, trapped, national).

% Argue for inclusive membership rules recognizing matrilineal descent and intermarried families, citing demographic decline (the community has shrunk for a century) as an argument that endogamy enforcement is now accelerating community extinction rather than preserving it. Publish, litigate, and organize but hold no formal authority over navjote or trust administration, which remains with priesthood and panchayats.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, reformist_parsis, excluded,
    organized, generational, constrained, national).

% Have adjudicated disputes over trust eligibility and navjote rights for children of intermarried Parsi women (notably in litigation over Valsad Parsi Anjuman and similar panchayat trust disputes), generally deferring to religious community autonomy over membership definition while occasionally intervening on charitable trust administration grounds.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_community_institutions).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Endogamy and ritual-validity rules coordinate a demographically small, geographically concentrated religious community's continuity: shared ritual practice, a bounded pool of co-religionists for marriage, and centralized administration of communal charitable and housing resources built up over generations specifically for community members.
% TRANSFER_FUNCTION: Moves standing, ritual eligibility, and access to communal trust resources (housing, education, charity) from those who marry outside the community (and disproportionately from their children and from Parsi women specifically) toward those who marry within it and toward the institutions that administer the boundary.
% ABSENT_VOICES: Children of mixed marriages have no voice in the marriage decision that determines their religious and economic eligibility. Parsi women who have already married out are structurally absent from panchayat governance that rules on their children's status. Reformist Parsis are organized and vocal but hold no formal authority over the interpretive rulings that would need to change.
% DISAPPEARANCE_RATIONALE: If endogamy enforcement and priestly gatekeeping of ritual validity vanished, communal trusts would need new eligibility criteria, fire temple access rules would change, intermarried families currently excluded would gain standing, and the community's demographic trajectory (already declining under strict enforcement, per multiple Parsi census studies) would shift — either toward stabilization through inclusion or, on the priesthood's own account, toward faster dissolution of a distinct identity.
% FOUNDING_PROBLEM: A small refugee community (Zoroastrians fleeing persecution in Persia, settling in Gujarat/India) needed to preserve religious, ritual, and cultural distinctiveness against assimilation pressure in a much larger host society, in a context where numbers were already limited and further ritual intermarriage was seen as existentially diluting.
% FOUNDING_PROBLEM_CORROBORATION: The priesthood and orthodox panchayats attest the founding problem (assimilation risk, ritual purity) remains live and cite continued small population size as justification. Independent demographers studying the Parsi community (whose population has fallen from roughly 114,000 in 1941 to under 60,000 in recent censuses) and reformist community members argue the enforcement mechanism itself is now the leading driver of decline, not intermarriage — a reading corroborated by demographic researchers outside the priesthood and by Indian government-funded 'Jiyo Parsi' program materials that treat low birth rates and exclusionary membership rules as the crisis, not intermarriage per se.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects the concrete costs borne by intermarrying Parsis and their children: denial of navjote, fire temple access, and trust benefits accumulated over generations. It is not higher because the coordination function (community continuity under real demographic threat) is not fabricated — the community's population decline is independently documented. Suppression (0.62) reflects that exit from the endogamy rule is not merely social disapproval but institutional: panchayat-administered trusts and fire temples make formal, binding eligibility determinations enforceable through property and religious-access law. Theater ratio is low-to-moderate and rising (0.28 at present) because a growing share of enforcement now serves boundary-maintenance symbolism (demographic decline continues despite strict enforcement) rather than the original assimilation-defense function — this is the metric substitution signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Priesthood and community institutions sit at the beneficiary end: they administer the boundary and its resources, and derive institutional relevance from scarcity of membership. Endogamous couples benefit straightforwardly. Intermarrying Parsis, their children, and especially Parsi women marrying out sit at the target end — the cost is concentrated, non-negotiable, and in the case of children, entirely non-consensual. The gender asymmetry (women's children penalized more than men's) is not incidental; it is a specific structural feature of the priesthood's interpretive rulings and is why parsi_women_marrying_out is authored as powerless/trapped rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assimilation risk to a persecuted refugee community) was genuinely live historically. The status is authored as contested rather than dead because population decline is real and ongoing, but the corroboration from outside the priesthood (independent demographers, government population-revival programs) increasingly attributes the decline to the enforcement mechanism itself rather than to insufficient enforcement — suggesting the tool built to solve the problem may now be accelerating it. This is precisely the founding_problem_status/disappearance_verdict mismatch structure the six-questions battery is designed to surface: status=contested pointing toward dead, verdict=world_rearranges, which should raise scrutiny of whether current enforcement is coordination or inertial extraction dressed as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_ratio_under_demographic_scarcity,
    'Does the endogamy rule still perform a net-positive coordination function for community survival, or has the extraction (cost to intermarrying members and their children) now exceeded whatever preservation benefit it produces, given continued population decline under strict enforcement?',
    'Comparative demographic analysis of Parsi communities/panchayats with varying degrees of intermarriage-inclusion policy (e.g., comparing Mumbai orthodox panchayats to more inclusive diaspora communities) tracking population trajectory, retention of youth, and community vitality indicators over multiple decades.',
    'If inclusive communities show better long-run population and cultural-continuity outcomes than strict-enforcement communities, this would support reclassifying the current enforcement regime as inertial extraction (piton-adjacent) rather than functioning coordination, strengthening the tangled_rope-toward-snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_under_demographic_scarcity, empirical, 'Whether endogamy enforcement still serves its stated preservation function or has become net-negative for community survival.').

omega_variable(
    gendered_asymmetry_doctrinal_necessity,
    'Is the asymmetric treatment of children of Parsi women versus Parsi men marrying out a doctrinally required feature of Zoroastrian law, or a patriarchal interpretive accretion that could be reformed without abandoning endogamy or priestly authority itself?',
    'Comparative textual and historical analysis of Zoroastrian scriptural sources on descent versus documented panchayat ruling patterns; examination of reform movements within Zoroastrianism (e.g., Iranian Zoroastrian communities with different descent rules) that maintain endogamy without the same gender asymmetry.',
    'If the asymmetry is interpretive rather than doctrinally required, the victim set (parsi_women_marrying_out) reflects an avoidable extraction layered onto a genuine coordination structure, sharpening the tangled_rope classification; if doctrinally required, the extraction is more deeply embedded in the coordination function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_asymmetry_doctrinal_necessity, conceptual, 'Whether the gender-asymmetric cost is a removable extraction layer or intrinsic to the reading''s doctrinal core.').

omega_variable(
    kernel_framing_membership_versus_marriage_validity,
    'Is the operative kernel here actually ''marriage validity'' (as in the sibling readings) or is it more precisely ''community membership eligibility,'' with marriage functioning merely as the triggering event? If the latter, does this reading belong in the family_law_authority kernel at all, or does it point to a distinct membership-boundary kernel that marriage-authority readings only partially overlap with?',
    'Structural comparison of panchayat rulings that adjudicate membership disputes not arising from marriage (e.g., conversion attempts, adoption) versus marriage-triggered disputes, to see whether the same doctrinal machinery and the same authority (priesthood/panchayat) governs both, or whether marriage is doctrinally distinct from other membership questions.',
    'If membership-boundary logic is the true kernel and marriage is merely its most visible trigger, this reading''s ε and structural data would need re-derivation against a membership_boundary_authority kernel instead of, or alongside, family_law_authority — a candidate future decomposition per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_membership_versus_marriage_validity, conceptual, 'Alternative framing: whether this reading is best modeled under family_law_authority or a distinct community-membership kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1947, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement(fami_tr_t1970, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(fami_tr_t2008, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(fami_be_t1947, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1947, 0.45).
narrative_ontology:measurement(fami_be_t1970, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement(fami_be_t2008, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2008, 0.57).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(fami_su_t1947, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1947, 0.5).
narrative_ontology:measurement(fami_su_t1970, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(fami_su_t2008, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five readings of the family_law_authority kernel, each authored as a structurally distinct constraint per the ε-invariance principle: the Parsi reading's endogamy/community-survival mechanism produces a different beneficiary/victim structure and a different ε than the sacramental (Hindu, Christian), contractual (secular), or Quranic-hadith (Muslim) readings, even though all five govern 'marriage' in the same national jurisdiction (India) and interact through shared civil courts and personal-law frameworks. Indian courts (an observer stakeholder here) adjudicate across all five readings, creating structural coupling: a ruling on communal trust eligibility in the Parsi reading can shift precedent read across sibling personal-law regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
