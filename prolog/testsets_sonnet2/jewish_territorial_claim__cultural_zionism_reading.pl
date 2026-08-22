% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Reading: Spiritual Center Without Sovereignty Requirement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Cultural Zionism, articulated most influentially by Ahad Ha'am, rejected
 *   Herzlian political Zionism's demand for sovereign statehood, arguing
 *   instead that Palestine should function as a spiritual-cultural homeland —
 *   a center of Hebrew revival, textual scholarship, and moral regeneration —
 *   that would nourish a dispersed diaspora without requiring conquest of
 *   political control or numerical dominance over the existing Arab
 *   population. In principle this reading opens space for binational
 *   coexistence and treats Arab presence as a demographic fact to be lived
 *   alongside rather than displaced. In practice, the cultural institutions
 *   this reading built — land purchases for schools and settlements,
 *   immigration facilitation for scholars and cultural workers, philanthropic
 *   capital flows — used much of the same infrastructure as the more
 *   explicitly territorial currents, and the binational proposals that would
 *   have tested this reading's stated logic (Brit Shalom) were marginalized
 *   within the wider movement well before 1948.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.38).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.32).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Reading: Spiritual Center Without Sovereignty Requirement").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'ecde1dae-194e-49fd-8902-a0f8626e01d6').
narrative_ontology:cs_kernel_codification('ecde1dae-194e-49fd-8902-a0f8626e01d6', distributed).
narrative_ontology:cs_authority_grounding('ecde1dae-194e-49fd-8902-a0f8626e01d6', distributed).
narrative_ontology:cs_reading_relation('ecde1dae-194e-49fd-8902-a0f8626e01d6', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecde1dae-194e-49fd-8902-a0f8626e01d6', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('ecde1dae-194e-49fd-8902-a0f8626e01d6', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('ecde1dae-194e-49fd-8902-a0f8626e01d6', foundational, spiritual_center_sufficient_without_sovereignty).
narrative_ontology:cs_axiom_status(spiritual_center_sufficient_without_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ecde1dae-194e-49fd-8902-a0f8626e01d6', spiritual_center_sufficient_without_sovereignty, conventional).
narrative_ontology:cs_axiom('ecde1dae-194e-49fd-8902-a0f8626e01d6', foundational, arab_presence_compatible_with_jewish_cultural_renewal).
narrative_ontology:cs_axiom_status(arab_presence_compatible_with_jewish_cultural_renewal, overridden).
narrative_ontology:cs_axiom_grounding('ecde1dae-194e-49fd-8902-a0f8626e01d6', arab_presence_compatible_with_jewish_cultural_renewal, empirically_contingent).
narrative_ontology:cs_reference_frame('ecde1dae-194e-49fd-8902-a0f8626e01d6', ahad_haam_spiritual_center_doctrine).
narrative_ontology:cs_drift_state('ecde1dae-194e-49fd-8902-a0f8626e01d6', post_1936_arab_revolt_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ecde1dae-194e-49fd-8902-a0f8626e01d6', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, yishuv_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_revivalist_educators).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities_seeking_spiritual_center).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_landholders).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_political_self_determination_claim).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, jewish_peoplehood_as_cultural_nation).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revival_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds Hebrew University, Hebrew-language schools, publishing houses, and cultural societies in Palestine, framing the project as spiritual and educational renewal rather than a claim to exclusive political rule. Draws funding and moral legitimacy from diaspora Jewish communities and depends on continued land access and immigration permits to sustain institutional growth, even while formally disclaiming demographic or sovereignty requirements.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, yishuv_cultural_institutions, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, yishuv_cultural_institutions, agenda_setter).

% Teach and codify modern Hebrew, train a new generation in a revived cultural vernacular, and gain professional standing and purpose from the cultural-center project. Their livelihoods and identity are tied to the ongoing legitimacy of a Jewish cultural presence in Palestine, though their explicit aim is renewal rather than territorial control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_revivalist_educators, beneficiary,
    moderate, biographical, constrained, regional).

% Support the Palestine cultural project from abroad as a source of meaning, textual and spiritual renewal, and cultural continuity, without themselves relocating or requiring political sovereignty to feel the project's benefit. Their exit option remains open — they can withdraw funding or attention without personal displacement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities_seeking_spiritual_center, beneficiary,
    moderate, generational, mobile, global).

% Experience land purchases, institution-building, and population inflow associated with the cultural project even though it disclaims sovereignty ambitions; the practical effect on land tenure, tenancy, and village life is similar in kind (if smaller in early scale) to other Zionist currents. Have no institutional voice in decisions about land transfer or institutional siting and cannot easily leave land that constitutes their livelihood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_landholders, payer,
    powerless, biographical, trapped, local).

% Represents the Arab national movement's claim to self-rule over the same territory. Even a binational or non-sovereign cultural framing structurally competes for demographic space, institutional resources, and eventual political configuration, since cultural infrastructure (schools, land purchase networks, immigration absorption capacity) becomes the substrate later political claims are built upon.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_political_self_determination_claim, payer,
    organized, generational, constrained, regional).

% Grants or withholds land purchase permits, immigration quotas, and institutional charters that make the cultural-center project possible at all. Adjudicates between competing claims and can expand or restrict the project's practical footprint independent of its stated cultural-only ambitions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_then_british_mandate_administration, agenda_setter,
    institutional, biographical, analytical, national).

% Jewish and Arab intellectuals who argue explicitly for shared or confederated political structures consistent with the cultural reading's own logic. Their proposals are marginalized within both Zionist institutional politics and Arab nationalist politics, leaving the binational potential this reading nominally opens largely untested in practice.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_advocates, excluded,
    powerless, generational, constrained, regional).

% Assess whether the cultural project functioned as a genuinely distinct political program or as an ideological vanguard and legitimating frame for demographic and territorial expansion pursued more explicitly by other Zionist currents.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, historians_of_zionism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared institutional and linguistic infrastructure — Hebrew revival, universities, publishing, textual scholarship — enabling a dispersed people to reconstitute cultural continuity and a spiritual home without requiring every participant to accept displacement of the existing population or forced majority-building.
% TRANSFER_FUNCTION: Moves land, institutional charters, immigration capacity, and international Jewish philanthropic resources into a growing Jewish cultural-institutional footprint in Palestine; correspondingly narrows Palestinian Arab control over land, demographic trajectory, and the eventual political shape of the territory, even absent an explicit sovereignty demand.
% ABSENT_VOICES: Palestinian Arab communities most directly affected by land transactions and settlement were rarely consulted on the siting or scale of cultural institutions; binational advocates who took the cultural reading's own logic seriously were sidelined by both Zionist and Arab nationalist mainstreams as political conditions hardened.
% DISAPPEARANCE_RATIONALE: Advocates of this reading would say Hebrew University, the language revival, and diaspora spiritual attachment to Palestine persist independent of any particular sovereignty outcome, so their disappearance would matter culturally but not politically. Critics — including from within the Zionist movement itself — argue the cultural project's land purchases, immigration facilitation, and institution-building were never actually separable from the demographic and territorial project other currents pursued more openly, so its removal would have altered land tenure and settlement trajectories materially.
% FOUNDING_PROBLEM: Diaspora Jewish communities lacked a living cultural and spiritual center capable of arresting assimilation and linguistic decline, and existing political Zionism's insistence on sovereignty and majority-building was seen by cultural Zionists (following Ahad Ha'am) as spiritually hollow and likely to provoke Arab resistance without addressing the deeper problem of Jewish cultural renewal.
% FOUNDING_PROBLEM_CORROBORATION: Cultural Zionist intellectuals themselves (Ahad Ha'am's circle, Brit Shalom figures) attested the problem as live and distinct from statehood. Independent historians of the Yishuv period note that in practice cultural institutions were financed and sited using the same land-acquisition and immigration infrastructure as political and labor Zionism, and that binational proposals consistent with the cultural reading's stated logic were never adopted by the movement's dominant institutions — corroboration from outside the cultural-Zionist camp is mixed to skeptical.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38, rising modestly to 1948) because this reading's explicit institutional program — universities, publishing, language revival — is comparatively low-displacement relative to sovereignty-seeking or maximalist currents, but land acquisition and immigration facilitation still transferred real resources and demographic trajectory away from existing Arab landholders and political claims, and that transfer grew as the broader movement's institutional infrastructure matured around it. Suppression is authored lower than for sovereignty-based readings (0.32) since this reading does not require displacing an existing majority or compelling acceptance by force — its own logic is compatible with continued Arab presence — but it is not zero, because enforcement of land transfer, permit administration, and institutional siting still occurred within a colonial administrative structure that did not consult Arab landholders. Theater ratio is moderate (0.28): cultural framing is substantively pursued (real universities, real language revival), not pure cover, but by 1948 the 'no sovereignty, no majority required' framing had drifted further from institutional practice than from stated ideology, since cultural-Zionist funded infrastructure fed directly into the demographic and territorial project other currents pursued more explicitly.
 *
 * PERSPECTIVAL GAP:
 *   From the cultural-Zionist institutional seat, the project reads as coordination — a shared cultural renewal serving a dispersed people, explicitly disclaiming the coercive sovereignty ambitions of political Zionism. From the Palestinian Arab landholder or self-determination-claim seat, the same land purchases, immigration facilitation, and institution-building appear as a substantially similar territorial and demographic encroachment executed under gentler branding, with the added frustration that no political acknowledgment of Arab presence's legitimacy accompanies the practical transfer. The engine's per-seat computation should register this divergence: an agenda_setter/beneficiary seat reading rope-like coordination against a payer seat reading tangled extraction through the same land-and-institution channel.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the Yishuv's cultural institutions, the Hebrew-revivalist educator class, and diaspora communities who gain spiritual and cultural goods without bearing displacement risk themselves — their directionality sits toward the beneficiary end, especially diaspora supporters who retain full mobility and never face the constraint's territorial consequences directly. Palestinian Arab landholders and the Arab self-determination claim are victims: they bear land transfer, demographic pressure, and institutional competition for the same territory, with landholders specifically trapped by their dependence on the land itself. The mandate administration functions as an intermediary agenda-setter whose permit and charter decisions determine how much of the cultural project's stated non-territorial ambition converts into practical land and demographic effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (diaspora spiritual/cultural decline) may remain partly live even where the sovereignty problem political Zionism addressed was resolved by 1948 through different means — meaning the cultural-Zionist mandate does not straightforwardly resolve into either 'fully live' or 'fully obsolete' the way a sovereignty-focused reading's founding problem would once a state exists. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: treating the cultural project as pure benign coordination (ignoring the land-transfer and demographic-competition effects it shared with other currents), and treating it as pure extraction indistinguishable from the sovereignty-seeking currents (ignoring its genuine, distinctively lower-displacement institutional program and its real, if unrealized, binational potential).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_political_zionism_separability,
    'Was cultural Zionism''s institutional program (universities, land purchase for schools, immigration facilitation for scholars) structurally separable from the demographic and territorial consolidation pursued by political and labor Zionism, or did it functionally feed the same land-transfer and demographic infrastructure regardless of its stated non-sovereignty aims?',
    'Archival tracing of land-purchase and immigration-permit records specifically attributable to cultural-Zionist institutions (Hebrew University, cultural societies) versus settlement-oriented Zionist organizations, to determine whether the resource flows were institutionally distinct or fungible within a shared Zionist funding and administrative apparatus.',
    'If genuinely separable, this reading''s lower extractiveness score is well-supported and the tangled_rope classification''s coordination component is robust. If the flows were fungible, effective extraction attributable to this reading specifically should be revised upward, closer to the labor_zionism_reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_political_zionism_separability, empirical, 'Whether cultural Zionism''s institution-building was practically distinct from or fungible with territorial-settlement Zionism''s resource base.').

omega_variable(
    binational_counterfactual_credibility,
    'Was the binational framework this reading opens toward (per Brit Shalom) ever a politically live possibility, or was it always a minority intellectual position with no realistic institutional path to adoption given the broader Zionist movement''s trajectory?',
    'Comparative study of Brit Shalom''s institutional influence, membership trajectory, and the specific decision points (e.g., Jewish Agency policy debates, response to the 1929 and 1936 uprisings) where binational proposals were considered and rejected, to assess counterfactual viability.',
    'If the binational path was genuinely foreclosed early by movement-wide institutional dynamics, this reading''s distinctiveness from political_zionism_reading becomes primarily rhetorical rather than structural, weakening its claim to a materially different extraction profile. If it remained live longer, the reading''s lower-extraction character is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_counterfactual_credibility, conceptual, 'Whether the binational potential this reading nominally opens was ever institutionally realizable within the Zionist movement.').

omega_variable(
    spiritual_center_claim_naturalization_risk,
    'Does framing the project as ''spiritual and cultural center'' function as a genuine distinct political program, or does it risk naturalizing continued land acquisition and immigration as culturally innocent in a way that obscures the same territorial competition present in sibling readings?',
    'Comparative discourse analysis of cultural-Zionist publications versus their actual land-acquisition and settlement-siting decisions across the mandate period, checking for divergence between stated cultural-only framing and practical territorial footprint.',
    'If the discourse substantially outpaced the practice, the theater_ratio and extractiveness scores should be read as conservative — this reading''s actual operation may have converged more with the other Zionist currents than its stated ideology suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_center_claim_naturalization_risk, conceptual, 'Whether the cultural framing understated the reading''s actual territorial and demographic effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1907, 0.18).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1927, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1927, 0.24).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1937, 0.26).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1897, 0.18).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1907, 0.24).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1917, 0.29).
narrative_ontology:measurement(jewi_be_t1927, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1927, 0.33).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1937, 0.36).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1897, 0.12).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1907, 0.16).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(jewi_su_t1927, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1927, 0.25).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1937, 0.29).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.1).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language label 'Zionism's claim to Palestine' per the ε-invariance principle: cultural_zionism_reading (this file, tangled_rope, moderate ε ~0.38), political_zionism_reading (sovereignty-requiring, higher ε), labor_zionism_reading (settlement-through-labor, higher ε and different beneficiary structure), and revisionist_zionism_reading (maximalist/military, highest ε and suppression). Each reading is authored as a structurally distinct constraint with its own beneficiary/victim sets, its own ε, and its own classification, because measuring 'Zionism' by sovereignty-demand versus cultural-institution-building versus military-compulsion yields incompatible ε values — this is the two-constraints signal from the ε-invariance test, resolved by decomposition rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__cultural_zionism_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
