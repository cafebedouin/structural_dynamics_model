% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Reading: Total Prohibition of Religious Imagery as Categorical Idolatry
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the iconoclast reading of the Decalogue's
 *   second-commandment kernel: the prohibition against graven images is read
 *   as covering ALL religious imagery without exception, such that any
 *   material representation used in worship — icon, mosaic, statue —
 *   constitutes idolatry regardless of the worshipper's intent to venerate
 *   rather than worship. This is a categorical, wall-type reading: there is
 *   no permitted class of material mediation of the holy. The reading is
 *   authored here as its own constraint with its own ε, distinct from the
 *   iconodule reading (which permits honor-through-image while forbidding
 *   only latria to the image itself) and the moderate reading (which permits
 *   two-dimensional images under regulation). The three readings are siblings
 *   in one kernel contest, not three measurements of one constraint — each
 *   has a different beneficiary/victim structure and a different
 *   classification, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - centralizing_imperial_authority: agenda_setter/beneficiary (institutional/arbitrage) — issues and enforces the total-prohibition edicts, consolidates ecclesiastical control
 *   - iconoclast_clergy_faction: beneficiary/agenda_setter (organized/mobile) — supplies doctrine, ascends to vacated sees
 *   - icon_producers: payer (moderate/trapped) — trade criminalized outright
 *   - monastic_communities: payer (organized/constrained) — lose devotional and economic center, face exile or confiscation
 *   - devotional_laity: payer (powerless/trapped) — personal devotional practice declared idolatrous
 *   - iconodule_theologians: excluded (organized/constrained) — deposed and silenced dissenting voices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading: Total Prohibition of Religious Imagery as Categorical Idolatry").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'a37774ec-d32d-4410-b63d-2d314c48ac12').
narrative_ontology:cs_kernel_codification('a37774ec-d32d-4410-b63d-2d314c48ac12', fixed_text).
narrative_ontology:cs_authority_grounding('a37774ec-d32d-4410-b63d-2d314c48ac12', lineage).
narrative_ontology:cs_interpretation_layer_present('a37774ec-d32d-4410-b63d-2d314c48ac12').
narrative_ontology:cs_reading_relation('a37774ec-d32d-4410-b63d-2d314c48ac12', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('a37774ec-d32d-4410-b63d-2d314c48ac12', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('a37774ec-d32d-4410-b63d-2d314c48ac12', foundational, material_mediation_of_holy_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_of_holy_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('a37774ec-d32d-4410-b63d-2d314c48ac12', material_mediation_of_holy_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('a37774ec-d32d-4410-b63d-2d314c48ac12', secondary, veneration_and_worship_are_not_theologically_separable).
narrative_ontology:cs_axiom_status(veneration_and_worship_are_not_theologically_separable, holdable).
narrative_ontology:cs_axiom_grounding('a37774ec-d32d-4410-b63d-2d314c48ac12', veneration_and_worship_are_not_theologically_separable, conventional).
narrative_ontology:cs_reference_frame('a37774ec-d32d-4410-b63d-2d314c48ac12', second_commandment_literal_prohibition).
narrative_ontology:cs_drift_state('a37774ec-d32d-4410-b63d-2d314c48ac12', post_iconoclast_council_consolidation, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('a37774ec-d32d-4410-b63d-2d314c48ac12', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues edicts declaring the prohibition absolute, convenes councils to ratify the reading, and deploys imperial troops and administrators to enforce image destruction. Gains a unified religious apparatus answerable directly to the throne rather than to monasteries and shrine networks that previously commanded independent loyalty and revenue. Frames the campaign as doctrinal purification while consolidating control over ecclesiastical appointments and property.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, beneficiary).

% A faction of bishops and theologians who supply the scriptural and patristic argument for total prohibition, ascend to sees vacated by deposed iconodule clergy, and staff the councils that formalize the reading into canon. Their theological authority and career advancement are tied directly to the prohibition's success.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction, agenda_setter).

% Painters, mosaicists, and craftsmen whose entire livelihood is the production of devotional images. Under the prohibition their trade is criminalized outright; workshops are shuttered, existing stock is destroyed, and practitioners face prosecution as purveyors of idolatry. They cannot simply retool to icon-adjacent work because the prohibition targets the representational act itself, not merely its abuse.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, trapped, regional).

% Monasteries whose liturgical, economic, and pilgrimage life is built around venerated images lose the material anchor of their communal identity and revenue base. Monks who resist are exiled, imprisoned, or executed; monastic estates tied to image cults are confiscated by the state. Some communities flee to peripheral or foreign territory, but many are geographically and institutionally bound to the contested sites.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, civilizational, constrained, regional).

% Ordinary worshippers whose personal and household devotional practice — prayer before icons, votive offerings, processions with venerated images — is declared idolatrous overnight. They lose access to devotional forms integral to their religious life, cannot appeal to a competing authority within the imperial church, and risk accusation if caught retaining images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_laity, payer,
    powerless, biographical, trapped, regional).

% Defenders of the incarnational argument for image veneration are deposed, exiled, or silenced by the councils that ratify the iconoclast reading. They continue to argue their case from exile or in hiding, but are structurally excluded from the imperial-sanctioned theological conversation that produced this reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    organized, generational, constrained, continental).

% A subsequent council convened to re-adjudicate the image question, reviewing the iconoclast period's councils, martyrologies, and theological arguments from a position of temporal distance. Its eventual ruling (favoring restoration of images) becomes the historical record against which this reading's claims are tested.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, later_ecumenical_council, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous, court-enforceable standard for what counts as permissible worship, eliminating theological disputes over degrees of veneration versus worship and giving the imperial center one clean criterion for orthodoxy across a religiously diverse empire.
% TRANSFER_FUNCTION: Moves ecclesiastical authority, monastic wealth, and control over religious meaning-making from shrine-centered and monastic networks to the imperial court and the clergy faction it elevates; moves devotional practice and livelihood away from icon producers, monastics, and laity toward state-sanctioned aniconic worship.
% ABSENT_VOICES: Iconodule theologians and the broader devotional laity who valued image-mediated worship are deposed, exiled, or simply overruled without their objections entering the councils that ratify this reading as binding doctrine.
% DISAPPEARANCE_RATIONALE: If the total-prohibition reading were abandoned, icon production would resume, monastic communities would recover their devotional and economic centers, and the imperial court would lose a key lever for centralizing ecclesiastical appointments — precisely what later happened when the reading was reversed at a subsequent council.
% FOUNDING_PROBLEM: A perceived crisis of idolatry: material images were held to have become objects of worship in themselves rather than aids to worship of the divine, understood by proponents as violating the second commandment and inviting divine judgment (interpreted by some as manifest in military and natural disasters).
% FOUNDING_PROBLEM_CORROBORATION: Imperial and iconoclast-clergy sources attest the idolatry crisis was real and urgent. Iconodule theologians, later monastic chroniclers, and the subsequent ecumenical council that reversed the prohibition attest from outside the benefiting faction that the 'crisis' was substantially a pretext for state control of ecclesiastical wealth and appointments, and that the incarnational theology of image veneration had never constituted the idolatry alleged.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 to 0.68 over the interval as the prohibition moves from proclamation to systematic enforcement — early edicts carry moderate extraction (loss of devotional access), but as monastic property confiscation, workshop destruction, and clergy purges intensify, the extractive transfer of wealth and authority to the imperial center becomes the dominant feature. Suppression rises sharply and early (0.50 to 0.85 by T=20-30) because the categorical reading requires immediate, comprehensive enforcement — there is no permitted class of image to leave alone, so every existing icon, mosaic, and statue is a compliance target from day one. Theater ratio climbs moderately (0.20 to 0.42) as councils convened to ratify the reading increasingly perform doctrinal consensus that masks the coercive deposition of dissenting bishops.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial/clergy-faction seat, the prohibition is a doctrinally necessary purification with genuine coordination value (settling generations of dispute over image use with one clean rule). From the payer seats — producers whose trade is criminalized, monastics whose communities are dismantled, laity whose devotional life is declared sin — the same structure computes as enforced extraction with no meaningful exit. The engine should surface this divergence directly from the structural data (power, exit options, beneficiary/victim declarations) rather than from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial authority and the clergy faction that rides the reading into vacated sees are structural beneficiaries — d near the beneficiary end — because the reading's success directly enlarges their institutional control and revenue capture. Icon producers, monastic communities, and devotional laity are targets with high d: their trade, communal life, and personal devotion are the material the prohibition extracts from, and none of them have meaningful exit (producers cannot retool, monastics are geographically bound to contested sites, laity have no alternative sanctioned church). Iconodule theologians are excluded rather than coordinated — their exclusion from the councils is definitional to how this reading was ratified, not incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a perceived idolatry crisis) is authored as contested rather than resolved: proponents insist it remains live, but the corroboration from outside the beneficiary set — later monastic chroniclers and the subsequent ecumenical council that reversed the prohibition — treats the 'crisis' as pretextual cover for a redistribution of ecclesiastical wealth and appointment power. This is the mandatrophy signature: a mandate (protecting the community from idolatry) persisting via imperial coercion after its evidentiary basis had, in the corroborating sources' own account, evaporated or never existed. Classifying this as tangled_rope rather than pure snare acknowledges the genuine coordination function it also serves (a single doctrinal standard reduces disputes over gradations of image use) while insisting the asymmetric extraction from producers, monastics, and laity, sustained by active enforcement, is equally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_determinacy,
    'Does the second-commandment text itself determine which reading — categorical prohibition, latria/dulia distinction, or the two-dimensional/three-dimensional split — is the correct interpretation, or is the text genuinely underdetermined such that the choice among readings is made on other grounds (political, cultural, institutional)?',
    'Philological and historical-critical analysis of the commandment''s original context (proscription of Ancient Near Eastern cult statuary) versus its later theological elaboration; comparison of patristic commentary across the period before the reading split hardened into rival factions.',
    'If the text is genuinely determinate toward the categorical reading, the iconoclast position has stronger claim to being a good-faith theological conclusion rather than a pretext; if underdetermined, the selection of this reading over its siblings is better explained by the imperial-centralization incentive documented in beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_determinacy, conceptual, 'Whether the commandment text determines the categorical reading or leaves it underdetermined.').

omega_variable(
    reading_selection_and_imperial_incentive,
    'Is the correlation between adoption of the categorical (iconoclast) reading and imperial administrative consolidation causal — did the emperor''s court select and promote this reading because of its centralizing effects — or merely coincidental with an independently arising theological movement?',
    'Examine the sequence and timing of imperial edicts relative to independent iconoclast theological writing; examine whether iconoclast clergy promotion patterns track loyalty to the imperial reading versus independent theological standing.',
    'If causal, this reading is better modeled as tangled_rope shading toward snare (extraction dominant, coordination function largely cover); if coincidental, the coordination function (doctrinal clarity) carries more independent weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_imperial_incentive, empirical, 'Whether imperial promotion of this reading was causally driven by its centralizing effect.').

omega_variable(
    victim_coalition_potential,
    'Could icon producers, monastic communities, and devotional laity have formed an effective resistance coalition despite their differing power levels (moderate, organized, powerless respectively), or does the geographic and institutional fragmentation of the empire prevent such coordination?',
    'Historical record of cross-regional resistance networks, clandestine icon veneration circles, and monastic communication during the enforcement period.',
    'Evidence of effective coalition formation would suggest the powerless/trapped classification for laity understates their structural leverage when aligned with organized monastic resistance; its absence would confirm the fragmentation the prohibition''s enforcement apparatus was partly designed to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_potential, empirical, 'Whether victim groups at different power levels could coordinate resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 60, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'second commandment image prohibition' per the ε-invariance principle. iconoclast_reading (this file) authors high extraction (0.68) and tangled_rope classification driven by categorical prohibition enforced against producers, monastics, and laity for imperial centralizing benefit. iconodule_reading authors low extraction consistent with rope or mountain-adjacent coordination (the latria/dulia distinction is presented by its proponents as clarifying rather than extractive, preserving devotional practice). moderate_iconoclast_reading sits between, restricting only three-dimensional statuary and authoring intermediate extraction. All three must be generated as separate files with independent ε values; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
