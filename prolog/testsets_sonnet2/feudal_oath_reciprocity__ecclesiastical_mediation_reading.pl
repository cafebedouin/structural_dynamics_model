% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Bounded by Ecclesiastical Sacramental Mediation
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the ecclesiastical-mediation reading of the
 *   feudal oath kernel: the church's sacramental framing of the oath as sworn
 *   before God, not merely between men, installs the church as an
 *   interpretive authority limiting what lords may extract from vassals, in
 *   exchange for tithes, endowments, and jurisdictional deference from both
 *   parties. This is a genuinely different constraint from a reading that
 *   sees the oath as authorizing near-unlimited lordly extraction
 *   (lord_extraction_reading) or one that sees the oath as a fixed,
 *   charter-bounded bargain enforced by text alone without ecclesiastical
 *   mediation (vassal_coordination_reading). Under this reading, ε is
 *   moderate: the church's mediating function is real and does constrain
 *   lords, but the mediation itself extracts (tithes, court fees, deference,
 *   land grants) and is unevenly available — protective mostly for nobles and
 *   knights, thin-to-absent for peasants.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: agenda-setter and co-beneficiary — defines and enforces the theological ceiling, collects tithes and jurisdiction as its price
 *   - parish_clergy_confessors: local beneficiary and informal mediator, dependent on the hierarchy above and on lordly goodwill below
 *   - great_lords: payer of the ceiling and beneficiary of the legitimacy it confers on their rule
 *   - lesser_vassals: payer of tithes and deference, partial beneficiary of the appeal mechanism
 *   - vassal_peasantry: payer at the bottom, structurally excluded from the forums where the ceiling is actually enforced
 *   - royal_authority: observer/excluded — a rival claimant to jurisdiction over the same disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Bounded by Ecclesiastical Sacramental Mediation").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '36a63f8c-8c44-4c76-a580-08acf5bbec5f').
narrative_ontology:cs_kernel_codification('36a63f8c-8c44-4c76-a580-08acf5bbec5f', distributed).
narrative_ontology:cs_authority_grounding('36a63f8c-8c44-4c76-a580-08acf5bbec5f', lineage).
narrative_ontology:cs_interpretation_layer_present('36a63f8c-8c44-4c76-a580-08acf5bbec5f').
narrative_ontology:cs_reading_relation('36a63f8c-8c44-4c76-a580-08acf5bbec5f', feudal_oath_reciprocity__lord_extraction_reading, influences).
narrative_ontology:cs_reading_relation('36a63f8c-8c44-4c76-a580-08acf5bbec5f', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('36a63f8c-8c44-4c76-a580-08acf5bbec5f', foundational, sacramental_oath_binds_conscience_beyond_secular_enforcement).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_conscience_beyond_secular_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('36a63f8c-8c44-4c76-a580-08acf5bbec5f', sacramental_oath_binds_conscience_beyond_secular_enforcement, theological).
narrative_ontology:cs_axiom('36a63f8c-8c44-4c76-a580-08acf5bbec5f', foundational, charity_imposes_a_ceiling_on_licit_lordly_demand).
narrative_ontology:cs_axiom_status(charity_imposes_a_ceiling_on_licit_lordly_demand, holdable).
narrative_ontology:cs_axiom_grounding('36a63f8c-8c44-4c76-a580-08acf5bbec5f', charity_imposes_a_ceiling_on_licit_lordly_demand, deontological).
narrative_ontology:cs_reference_frame('36a63f8c-8c44-4c76-a580-08acf5bbec5f', sacramental_oath_binds_before_god).
narrative_ontology:cs_drift_state('36a63f8c-8c44-4c76-a580-08acf5bbec5f', high_medieval_canon_law_formalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36a63f8c-8c44-4c76-a580-08acf5bbec5f', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, parish_clergy_confessors).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_peasantry).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, great_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, great_lords).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_binds_conscience).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, charity_limits_lordly_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and church courts assert that the feudal oath is sacramentally binding — sworn on relics, witnessed by clergy, understood as a promise before God. This lets the church define what counts as an 'excessive' or 'uncharitable' exaction, adjudicate disputes between lord and vassal in ecclesiastical courts, threaten excommunication or interdict against lords who breach the theological limits, and extract tithes, endowments, and deference from both parties in exchange for legitimating the arrangement at all. The church's own extraction rides underneath its role as limiter of secular extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary).

% Local priests hear confession from both lords and vassals, administer the sacramental weight that makes oath-breaking a sin rather than merely a breach of contract, and mediate local disputes informally before they reach ecclesiastical courts. They gain moral authority and material support (tithes, glebe land) from occupying this mediating position, but have little independent power to enforce rulings against a resistant lord.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, parish_clergy_confessors, beneficiary,
    moderate, biographical, constrained, regional).

% Lords swear the oath and receive service, but the sacramental framing genuinely constrains what they can demand — excessive exaction risks not just vassal revolt but spiritual censure, interdict on their lands, and loss of legitimacy with their own peasantry who hear the church's teaching on charitable lordship. They also benefit: the church's blessing of the hierarchy as divinely sanctioned dampens vassal resistance more effectively than force alone would. Exit from the arrangement (renouncing the oath's sacramental character) is possible but costly — it would strip legitimacy from their own rule.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, great_lords, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, great_lords, beneficiary).

% Knights and lesser lords who owe service upward are protected somewhat by the theological ceiling on what a lord may extract, and can appeal to ecclesiastical courts or local clergy when a lord's demands appear uncharitable. But they also pay: tithes to the church, deference to clerical authority, and acceptance that their own obligations are likewise sacramentally binding and cannot simply be renounced when inconvenient.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals, beneficiary).

% Peasant tenants at the bottom of the hierarchy receive the thinnest protection from the theological ceiling — the doctrine of charitable lordship is invoked mainly in disputes between lord and knight, rarely enforced on their behalf against day-to-day exaction. They pay tithes to the church on top of feudal dues to the lord, and have no practical access to ecclesiastical courts; their only defense is informal, dependent on a sympathetic local priest willing to intercede.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_peasantry, payer,
    powerless, biographical, trapped, local).

% Kings watch the church's growing jurisdiction over feudal disputes with ambivalence — it can restrain overmighty lords in ways useful to the crown, but it also asserts a competing source of legitimacy and a parallel court system the king does not control. Royal courts periodically try to claim jurisdiction over the same disputes, producing recurring church-state friction (e.g., disputes over benefit of clergy, investiture).
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, royal_authority, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, royal_authority, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The sacramental framing of the feudal oath provides a shared, third-party-enforced standard for what counts as legitimate versus excessive lordly demand — a genuine coordination good in a world without strong secular courts, since it gives vassals and lesser lords a language and a forum (confession, ecclesiastical court, clerical intercession) in which to contest exactions that would otherwise be settled by force alone.
% TRANSFER_FUNCTION: Moves interpretive authority and a share of surplus (tithes, endowments, deference, land grants) from both lords and vassals toward the ecclesiastical hierarchy and local clergy, in exchange for the church's mediating and limiting function; simultaneously moves a partial, unevenly enforced protection from lords toward knights and (much more thinly) toward peasants.
% ABSENT_VOICES: Vassal peasantry, who bear feudal dues and tithes alike, are almost entirely absent from the fora where the theological limits are actually litigated — ecclesiastical courts overwhelmingly hear disputes among lords, knights, and the church itself, not smallholders' grievances. Their interests are represented, if at all, only through the moral rhetoric of charitable lordship preached from the pulpit, not through any forum they can access.
% DISAPPEARANCE_RATIONALE: If the sacramental character of the oath were stripped away overnight, lords would lose the reputational and spiritual check on extraction, ecclesiastical courts would lose jurisdiction over feudal disputes, the church would lose a major channel of tithe and endowment collection tied to its mediating role, and lesser vassals would lose their only quasi-institutional forum for contesting lordly demands — the entire dispute-resolution architecture of the high medieval feudal order would need to be rebuilt on purely secular or purely coercive terms.
% FOUNDING_PROBLEM: In the absence of strong centralized secular courts, feudal relationships needed some third-party-recognized standard to distinguish legitimate lordly authority from naked extraction, and some mechanism to make oath-breaking costly to both parties — the church supplied both by sacralizing the oath and installing itself as interpreter of its limits.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and later canon lawyers attest the mediating function was real in high-profile disputes among the nobility (e.g., baronial revolts framed partly in terms of a lord's broken faith); but manorial court records and peasant petition surviving from the same period, read by economic historians outside the church's own record-keeping, show almost no cases where the theological ceiling protected a peasant tenant from exaction — the corroboration for the doctrine's protective function thins sharply the further one moves from the noble strata that produced the surviving records.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48 at interval end) is moderate rather than low or high because two genuine functions coexist: a real coordination good (a recognized, third-party-enforced standard limiting arbitrary lordly demand, which most feudal relationships lacked) and a real extraction (the church's own take in tithes, court fees, land endowments, and the deference required to maintain its interpretive monopoly). Suppression (0.55) reflects the coercive apparatus behind the sacramental claim — excommunication, interdict, denial of sacraments — which is a real, escalating enforcement mechanism, not mere rhetoric, though it grows more elaborate (rising suppression_requirement) as canon law courts formalize over the interval. Theater ratio (0.30) is moderate: some of the mediating apparatus (elaborate oath ceremonies, relic-swearing) is performative reinforcement of legitimacy rather than functional dispute-resolution, but the underlying court system did adjudicate real cases.
 *
 * PERSPECTIVAL GAP:
 *   A great lord experiences this constraint as a moderate but real constraint on his freedom of action, paired with a legitimacy benefit he could not otherwise purchase — the engine should compute something closer to symmetric for that seat. A peasant tenant experiences the same doctrinal structure as an additional layer of extraction (the tithe) with no practical corresponding protection — the engine should compute that seat much closer to full target. The gap is not a measurement error; it is the structural fact that 'the feudal oath's ecclesiastical limits' meant something different depending on how far up the hierarchy the disputing party stood.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy sits closest to the beneficiary end: it sets the interpretive terms, collects the tithe and endowment stream, and has the most mobile exit (a bishop's institutional position outlives any single lord-vassal relationship). Great lords and lesser vassals sit near symmetric — both pay into the arrangement (tithes, deference, submission to ecclesiastical jurisdiction) and both draw real benefit (the lords' rule gains legitimacy; the lesser vassals gain an appeal mechanism), so their directionality is mixed rather than purely extractive or purely beneficial. Vassal peasantry sits closest to the target end: they pay tithes and feudal dues alike and have essentially no practical access to the mediating function that is supposed to protect them, which is why they are declared victims rather than mixed beneficiaries despite the doctrine nominally applying to them too.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no secular forum to check arbitrary lordly extraction) was genuinely live in the tenth and eleventh centuries and remained partially live for knights and lesser lords well into the interval, which is why founding_problem_status is authored as contested rather than flatly dead: the mediating function did real work for the strata whose disputes reached ecclesiastical courts, even as it calcified into a revenue and jurisdiction stream for the church and thinned to near-irrelevance for the peasantry it nominally covered. Classifying this as tangled_rope rather than snare preserves the genuine coordination component (a real, functioning alternative to pure force existed for the upper strata) while still registering the asymmetric extraction (tithes and deference extracted from a peasantry that received little of the protective benefit) that a pure-rope reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_constraint_vs_legitimating_veneer,
    'Did the theological ceiling on lordly extraction meaningfully bind lords'' actual behavior, or did it function mainly as a legitimating narrative that was invoked rhetorically but rarely enforced against a determined lord?',
    'Comparative study of documented cases where ecclesiastical courts or clerical intercession actually reversed or moderated a lord''s exaction, weighted against the total volume of extraction that occurred without any recorded ecclesiastical check, across regions and time periods within the interval.',
    'If enforcement was rare relative to total extraction, this reading''s tangled_rope classification should shift toward snare (the coordination story is mostly cover); if enforcement was common and consequential for the noble and knightly strata, tangled_rope with a real coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_constraint_vs_legitimating_veneer, empirical, 'Whether the sacramental ceiling was a functioning constraint or primarily rhetorical legitimation.').

omega_variable(
    reading_selection_and_source_bias,
    'The choice of the ecclesiastical-mediation reading over the lord_extraction or vassal_coordination readings for this constraint''s ε and structure was guided by chronicle and canon-law sources produced substantially by the clergy and nobility themselves — does the surviving evidentiary base systematically favor this reading over its siblings by over-representing cases where mediation was invoked?',
    'Cross-reference against non-ecclesiastical sources (manorial accounts, secular charters, archaeological evidence of peasant material conditions) to see whether the mediating function''s apparent prevalence in the clerical record is representative or an artifact of who kept records.',
    'If the mediation function is over-represented in the source base relative to lived experience, this reading''s beneficiary/victim structure and ε may overstate the protective function for lower strata and understate the pure-extraction character the lord_extraction_reading emphasizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_source_bias, conceptual, 'Whether source bias toward clerical and noble records inflates the apparent reach of ecclesiastical mediation.').

omega_variable(
    church_as_natural_vs_constructed_authority,
    'Is the church''s interpretive authority over the oath a natural consequence of the oath''s sacramental character (the oath simply IS a promise before God, and someone must interpret that), or a constructed jurisdictional claim the church built and defended for its own institutional benefit?',
    'Trace the historical emergence of ecclesiastical jurisdiction over lay oaths — was sacramental framing present from the earliest feudal oaths, or was it progressively layered on as canon law courts expanded their jurisdictional reach in the eleventh and twelfth centuries (e.g., alongside the Gregorian Reform)?',
    'If the sacramental framing was a later institutional construction rather than an original feature of the oath, the church''s beneficiary status looks more like captured jurisdiction than natural theological consequence, strengthening the case for treating ecclesiastical extraction as the dominant component rather than an incidental cost of genuine mediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_as_natural_vs_constructed_authority, conceptual, 'Whether ecclesiastical authority over the oath is intrinsic to its sacramental nature or a constructed jurisdictional expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1050, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.18).
narrative_ontology:measurement(feud_tr_t1110, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1110, 0.2).
narrative_ontology:measurement(feud_tr_t1170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1170, 0.24).
narrative_ontology:measurement(feud_tr_t1230, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1230, 0.27).
narrative_ontology:measurement(feud_tr_t1290, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1290, 0.29).
narrative_ontology:measurement(feud_tr_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1350, 0.3).

% Extraction over time
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.32).
narrative_ontology:measurement(feud_be_t1110, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1110, 0.36).
narrative_ontology:measurement(feud_be_t1170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1170, 0.4).
narrative_ontology:measurement(feud_be_t1230, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1230, 0.44).
narrative_ontology:measurement(feud_be_t1290, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1290, 0.46).
narrative_ontology:measurement(feud_be_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1350, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.42).
narrative_ontology:measurement(feud_su_t1110, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1110, 0.46).
narrative_ontology:measurement(feud_su_t1170, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1170, 0.49).
narrative_ontology:measurement(feud_su_t1230, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1230, 0.52).
narrative_ontology:measurement(feud_su_t1290, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1290, 0.54).
narrative_ontology:measurement(feud_su_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1350, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the feudal_oath_reciprocity kernel. lord_extraction_reading treats the same oath as authorizing near-unbounded extraction bounded only by vassal capacity (high ε, snare-leaning). vassal_coordination_reading treats it as a fixed charter-bounded bargain enforced by text without ecclesiastical mediation (lower ε, rope-leaning). This reading (ecclesiastical_mediation_reading) sits between them: moderate ε, tangled_rope, because it adds a genuine but self-interested third-party mediator whose interpretive authority both constrains lords and extracts its own rent. All three share the same underlying oath-swearing practice but diverge on who holds interpretive authority and what that authority does with it — they are not the same constraint measured differently; they are three different constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
