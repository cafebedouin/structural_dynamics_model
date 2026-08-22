% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)
 *   domain: social/religious/political-economy
 *
 * SUMMARY:
 *   This constraint story instantiates the ORTHODOX TEXTUAL READING of the
 *   contested jati-practice-norm kernel. Under this reading, jati boundaries
 *   are derived from a fixed scriptural varna framework (the four
 *   divinely-ordained functional categories: brahmin, kshatriya, vaishya,
 *   shudra) and deviation from assigned occupational roles is understood as
 *   ritual pollution that contaminates the deviant and requires expiation.
 *   The reading asserts the cosmological truth and necessity of the
 *   framework. This reading coexists historically with alternative readings:
 *   localized-practice readings that treat jati boundaries as subject to
 *   continuous local renegotiation (OQ-254 sibling:
 *   jati_practice_norm__localized_practice_reading), and colonial-census
 *   readings that identify jati stabilization as an artifact of
 *   administrative enumeration (OQ-254 sibling:
 *   jati_practice_norm__colonial_census_reading). The orthodox textual
 *   reading produces the highest measured extractiveness of the three
 *   siblings because it justifies immobility through pollution doctrine and
 *   benefits maximally from categorical rigidity.
 *
 * KEY AGENTS:
 *   - Brahminical priesthood: institutional authority, sets varna interpretation and pollution standards, benefits materially from ritual monopoly and jati-segregated ceremonies
 *   - Landed upper jatis (kshatriyas, wealthy vaishyas): beneficiaries of varna-justified land control and labor coercion, maintain political authority through scriptural legitimation
 *   - Polluted-occupation jatis (tanners, leatherworkers, butchers): victims of occupational confinement and stigma, identity-locked by fusion of jati identity with polluting work
 *   - Untouchable castes: victims of pollution-doctrine enforcement, trapped outside all legitimate occupational and ritual spaces
 *   - Women in lower jatis: victims of intersecting jati and gender-based extraction, doubly constrained by occupational boundaries and patriarchal control
 *   - Reform movements (nineteenth-twentieth centuries): excluded from orthodox authority structure, challenge the framework through alternative readings and social equality frames
 *   - Colonial administrators: analytical seat, operationalize jati categories for governance without endorsing the orthodox textual legitimacy frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.82).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.78).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political-economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'af5ed753-30a8-4b61-9a75-c2e8feef5e83').
narrative_ontology:cs_kernel_codification('af5ed753-30a8-4b61-9a75-c2e8feef5e83', fixed_text).
narrative_ontology:cs_authority_grounding('af5ed753-30a8-4b61-9a75-c2e8feef5e83', lineage).
narrative_ontology:cs_interpretation_layer_present('af5ed753-30a8-4b61-9a75-c2e8feef5e83').
narrative_ontology:cs_reading_relation('af5ed753-30a8-4b61-9a75-c2e8feef5e83', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('af5ed753-30a8-4b61-9a75-c2e8feef5e83', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('af5ed753-30a8-4b61-9a75-c2e8feef5e83', foundational, varna_boundaries_scripturally_immutable).
narrative_ontology:cs_axiom_status(varna_boundaries_scripturally_immutable, holdable).
narrative_ontology:cs_axiom_grounding('af5ed753-30a8-4b61-9a75-c2e8feef5e83', varna_boundaries_scripturally_immutable, deontological).
narrative_ontology:cs_axiom('af5ed753-30a8-4b61-9a75-c2e8feef5e83', foundational, occupational_deviation_is_ritual_pollution).
narrative_ontology:cs_axiom_status(occupational_deviation_is_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('af5ed753-30a8-4b61-9a75-c2e8feef5e83', occupational_deviation_is_ritual_pollution, conventional).
narrative_ontology:cs_axiom('af5ed753-30a8-4b61-9a75-c2e8feef5e83', secondary, brahminical_interpretive_authority_binding).
narrative_ontology:cs_axiom_status(brahminical_interpretive_authority_binding, holdable).
narrative_ontology:cs_axiom_grounding('af5ed753-30a8-4b61-9a75-c2e8feef5e83', brahminical_interpretive_authority_binding, conventional).
narrative_ontology:cs_reference_frame('af5ed753-30a8-4b61-9a75-c2e8feef5e83', vedic_varna_cosmology_applied).
narrative_ontology:cs_drift_state('af5ed753-30a8-4b61-9a75-c2e8feef5e83', colonial_and_post_independence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('af5ed753-30a8-4b61-9a75-c2e8feef5e83', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahminical_priesthood).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, landed_upper_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, polluted_occupation_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, untouchable_castes).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, women_in_lower_jatis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, women_in_lower_jatis).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, scriptural_varna_cosmology).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, occupational_inheritance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains textual authority over varna interpretation and ritual purity standards. Sets the framework through scriptural exegesis, validates jati classifications, pronounces on pollution and expiation. Collects material support through priestly fees and land grants dependent on jati-segregated ritual transactions. Their authority depends on categorical fixity — any local deviation or boundary renegotiation threatens their interpretive monopoly.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Hold hereditary land and political authority justified by varna-assigned superiority. Their social position and economic control are legitimized by the scriptural framework and depend on the immobility of lower jatis. They benefit from labor coercion structured through ritual prohibition of occupational mobility and enforcement of subordination norms.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, landed_upper_jatis, beneficiary,
    powerful, generational, arbitrage, regional).

% Assigned occupations classified as ritually polluting (tanning, leatherwork, butchery, refuse handling) by scriptural varna logic. Their jati identity is fused with the occupation; leaving it is framed as spiritual transgression and social death. They bear the burden of stigma and bear material extraction through exclusion from higher-status occupations. Their situation is perpetual because the framework treats occupational inheritance as cosmically ordained.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, polluted_occupation_jatis, payer,
    powerless, biographical, identity_locked, regional).

% Occupy the position of ritual pollution itself — their touch and presence are deemed defiling under orthodox textual reading. Excluded from temples, wells, and upper-jati spaces. Confined to the most degrading labor (cremation, corpse handling, refuse removal). Their very existence is structured as contamination. Exit requires complete renunciation of local identity and relocation; staying means perpetual subordination.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, untouchable_castes, payer,
    powerless, biographical, trapped, regional).

% Bear intersecting extraction: jati-assigned occupational exclusion and gendered access restrictions. Subject to both ritual purity norms and patriarchal control; their mobility constrained by jati boundaries and gender norms enforced through the same authority structure. They receive minimal benefit from jati coordination and face compounded extraction.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, women_in_lower_jatis, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, women_in_lower_jatis, beneficiary).

% Nineteenth and twentieth-century movements challenging jati rigidity and pollution doctrine through alternative scriptural readings, rationalist critique, or social equality frames. Are systematically opposed by orthodox authorities through social boycott, moral denunciation, and institutional exclusion from ritual legitimacy. Their presence is structurally outside the orthodox framework's adjudicative spaces.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reform_movements, excluded,
    organized, generational, constrained, regional).

% Engaged with jati boundaries through census enumeration and administrative categorization. External to the orthodox textual authority structure but operationalize jati categories for governance. Document and interact with the constraint from outside its legitimacy frame.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, colonial_administrators, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, brahminical_priesthood).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The varna framework provides a cosmologically-framed model of occupational specialization and ritual complementarity: each jati occupies a hereditary role in a theologically integrated system where brahminical ritual mediation, warrior governance, merchant production, and service labor are coordinated through scriptural role assignment. The coordination claim is that occupational differentiation requires stability and that varna provides the stability frame.
% TRANSFER_FUNCTION: Moves material wealth (agricultural surplus, rents, temple donations, labor services) upward to brahminical priests and upper-jati landholders through: (1) ritual taxation (priestly fees for lifecycle ceremonies monopolized by brahminical authority), (2) labor coercion (lower jatis confined to occupations that generate less wealth and require servile status), (3) land control (upper jatis claim ownership justified by varna-assigned superiority), and (4) surplus extraction through jati-segregated labor relations where mobility out of assigned occupations is forbidden.
% ABSENT_VOICES: Lower-jati dissidents, reformers challenging the scriptural reading, merchants and artisans seeking occupational mobility, untouchable castes seeking pollution-doctrine rejection, women seeking gender-jati boundary crossing, localized practitioners whose boundary negotiations contradict textual fixity—all are structurally excluded from the orthodox framework's legitimate adjudicative spaces. They would testify that boundaries are locally negotiable, that pollution is a social construction, that occupational inheritance is coercive, not cosmically ordained. The framework silences them by declaring dissent itself a form of pollution.
% DISAPPEARANCE_RATIONALE: If the orthodox textual reading vanished (replaced by localized practice readings or equality-based reforms), the regulatory structure maintaining occupational immobility would collapse. Lower jatis would immediately seek occupational mobility; the material surplus flowing to upper jatis and priests would decline; land tenure systems justified by varna superiority would face challenge; ritual monopolies would erode. The entire political economy depends on categorical enforcement—the constraint's disappearance would require reorganization of property, labor, and authority.
% FOUNDING_PROBLEM: Complex agrarian societies require occupational specialization and stable labor allocation; varna provides a theological explanation for why certain people remain in certain occupations across generations, legitimizing hereditary occupational assignment and justifying the authority of those assigned to governance and priesthood roles.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox brahminical authorities attest the founding problem is live: occupational specialization remains necessary and varna remains the legitimate frame for stability. Lower-jati reformers and comparative scholars attest the founding problem is solved: occupational specialization exists in other societies without rigid jati boundaries, mobility is empirically possible, and varna serves as ideology justifying extraction rather than as a necessary coordination mechanism. Colonial-era census data and economic historians document the reorganization of labor after jati boundaries loosened, supporting the contested reading.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 endpoint) because the constraint confines large populations to low-wealth occupations through a pollution ideology that makes exit not merely forbidden but spiritually transgressive. The measurement series shows extraction rising steeply from 0.68 to 0.82 over the first 25 time units, then plateauing—consistent with a constraint whose extractive force is fixed by the text but whose application (enforcement intensity) responds to resistance. Suppression is high (0.78 endpoint) because the constraint's persistence depends on active enforcement: ritual sanctions for boundary crossing (exclusion from temples, caste council punishment, social death), legal support from upper-jati magistrates, violence against lower jatis claiming mobility. Theater ratio rises from 0.25 to 0.42 and then stabilizes—the early rise reflects increasing performative ceremonialism (elaborate pollution avoidance rituals) as resistance strengthens; the plateau reflects the constraint settling into a stable enforcement posture where theater (pollution-avoidance performance) and extraction (actual labor coercion) reach equilibrium. The accessibility-collapse metric (0.81) reflects that the orthodox framework makes alternatives nearly unthinkable within its own logic—to exit is to accept permanent spiritual pollution, a cost the framework itself defines as infinite. Resistance (0.71) is substantial because lower jatis and reform movements continuously challenge the boundaries through local practice, occupational migration, and ideological counter-claims. The measurements span a single shared time grid so every metric is valued at every time point; no metric substitution occurs.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (brahminical priesthood) and beneficiary (upper jatis) seats experience this as genuine coordination—the varna framework legitimately allocates roles and ensures stability. The payer seats experience it as pure extraction—occupational confinement backed by pollution ideology and enforced through violence and spiritual threat. The engine computes seat-specific classifications from the structural data: the priest seat derives low effective extraction (because they are coded as beneficiary, agenda-setter, institutional power, arbitrage exit) while the polluted-occupation and untouchable seats compute high effective extraction (victims, powerless, identity-locked exit). This divergence is the measurement the constraint story exists to capture—the same rule looks like coordination to those it benefits and coercion to those it harms, and that asymmetry is the snare structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical priesthood: d ≈ 0.1 (full beneficiary)—they set the rules, control the interpretation, collect material benefit from ritual monopoly, have arbitrage-level exit (can move to other regions and maintain priestly status). Landed upper jatis: d ≈ 0.15 (strong beneficiary)—they benefit from labor coercion and land-ownership justification, have powerful institutional resources and regional arbitrage options. Polluted-occupation jatis: d ≈ 0.88 (strong target)—they bear the cost of occupational confinement, have powerless-atom power, identity-locked exit (leaving means spiritual death in the traditional structure), generate the extracted surplus. Untouchable castes: d ≈ 0.95 (nearly full target)—they are the targets of maximum extraction, have powerless power, trapped exit (no legitimate occupational niche at all), and face violent enforcement. Women in lower jatis: d ≈ 0.82 (strong target)—compound victimization from jati and gender, identity-locked exit through both jati fusion and patriarchal family structure, constrained power. Reform movements: d ≈ 0.50 (symmetric)—they are excluded and opposed but not directly extracted from by the constraint (they are external challengers, not internal subjects). Colonial administrators: d ≈ 0.50 (analytical, symmetric by definition). These directionalities follow from the beneficiary/victim declarations and exit-option assignments; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (occupational specialization requiring stability) is CONTESTED: the orthodox reading claims it is still live (specialization still needs varna stability), while reformers and empirical observation claim it is dead (specialization works fine in societies without rigid jati boundaries). The disappearance verdict (world_rearranges) establishes that the constraint is NOT a natural law—if it vanished, arrangements would reshape around occupational mobility. This combination (contested founding problem + world_rearranges verdict) triggers mandatrophy detection: the constraint persists because beneficiaries maintain it through enforcement, not because the coordination problem it was built for requires it. The high extractiveness (0.82) and high suppression (0.78) confirm the snare classification: the constraint's primary function is extraction, and its coordination story is secondary. The theater ratio's rise and plateau pattern further supports snare classification: as resistance grows, the framework increases performative pollution avoidance (theater) to maintain psychological compliance, while material extraction remains steady. The constraint is a living snare, not a dying rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pollution_doctrine_internalization,
    'Is the measured suppression primarily structural (backed by institutional sanctions and violence) or internalized (belief in spiritual pollution that persists after enforcement removal)?',
    'Historical evidence from post-independence India where legal enforcement of jati-based discrimination was prohibited but occupational mobility remained constrained in some regions—if suppression persists despite enforcement removal, it is partially internalized. Compare regions of strong reform vs. weak reform to measure the persistence gradient.',
    'If substantially internalized, the effective suppression the constraint imposes is higher than the structural measure suggests, and targets carry the suppression with them if they achieve physical exit. If primarily structural, alternative readings (localized_practice_reading) might be more viable through enforcement removal alone. Internalization changes the classification from snare-at-enforcement to snare-with-psychological-embedding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pollution_doctrine_internalization, empirical, 'Structural vs. internalized suppression in pollution-doctrine enforcement').

omega_variable(
    textual_fixity_vs_interpretive_drift,
    'Does the ''fixed scriptural varna framework'' actually remain fixed across orthodox exegetical history, or does orthodox interpretation itself drift while claiming continuity?',
    'Detailed comparison of Manusmriti commentaries (Kulluka, Medhatithi, later exegetes) and Dharmaśāstra texts from different periods: if occupational classifications, pollution rules, and remediation procedures change substantially while authorities claim textual fidelity, the ''fixity'' is a framing claim, not an empirical fact.',
    'If interpretation drifts substantially, the orthodox textual reading itself contains latent localized-practice-reading logic underneath the fixity narrative. This would make the reading partially foreclose the localized reading (orthodox claims fixity) while actually instantiating it (interpretation evolves). The classification might shift from pure snare to tangled_rope if the framework secretly coordinates occupational adaptation while performing fixity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fixity_vs_interpretive_drift, empirical, 'Whether ''fixed scripture'' claim masks interpretive flexibility').

omega_variable(
    brahminical_authority_dependence,
    'Does the constraint depend on brahminical priesthood institutional maintenance (authority structure requires active lineage transmission), or would it persist through generalized social belief in varna even without organized priesthood?',
    'Historical cases where brahminical institutional control weakened (colonial suppression of priest endowments, modern Hindu reformation challenging priest monopoly) but occupational boundaries persisted—indicates distributed belief rather than institutional dependence. Conversely, regions where institutional priesthood maintained—indicates institutional dependence.',
    'If primarily institutionally dependent, removing the priesthood''s structural position (land grants, ritual monopoly, interpretive authority) could degrade the constraint to piton status (coasting on cultural inertia without concentrated beneficiary maintenance). If distributed belief is primary, institutional reform alone is insufficient; ideological transformation (reformation movements) would be necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_authority_dependence, empirical, 'Whether constraint depends on institutional priesthood or distributed cultural belief').

omega_variable(
    kernel_reading_foreclosure,
    'Is the orthodox textual reading logically incompatible with the localized_practice_reading (both cannot be held in one framework), or do they represent incommensurable framings that can coexist in different institutional spaces?',
    'Test whether an orthodox practitioner can coherently hold both ''varna categories are scripturally fixed'' AND ''local communities continuously renegotiate boundaries'' in the same mental framework. If the two claims are internally contradictory at the logic level, foreclosure holds; if they can be compartmentalized (textual ideal vs. pragmatic practice), coexistence holds.',
    'If foreclosure: the orthodox and localized readings are genuinely incompatible; one reading''s triumph requires the other''s defeat. If coexistence: both readings persist as held by different parties/contexts; institutional competition continues indefinitely. The reading_relations value (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between orthodox-textual and localized-practice readings of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(jati_tr_t0, observed).
narrative_ontology:measurement(jati_tr_t5, jati_practice_norm__orthodox_textual_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(jati_tr_t5, observed).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__orthodox_textual_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(jati_tr_t10, observed).
narrative_ontology:measurement(jati_tr_t15, jati_practice_norm__orthodox_textual_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(jati_tr_t15, observed).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(jati_tr_t20, observed).
narrative_ontology:measurement(jati_tr_t25, jati_practice_norm__orthodox_textual_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(jati_tr_t25, observed).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__orthodox_textual_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(jati_tr_t30, observed).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(jati_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(jati_be_t0, observed).
narrative_ontology:measurement(jati_be_t5, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(jati_be_t5, observed).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(jati_be_t10, observed).
narrative_ontology:measurement(jati_be_t15, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(jati_be_t15, observed).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(jati_be_t20, observed).
narrative_ontology:measurement(jati_be_t25, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(jati_be_t25, observed).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(jati_be_t30, observed).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(jati_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(jati_su_t0, observed).
narrative_ontology:measurement(jati_su_t5, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(jati_su_t5, observed).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(jati_su_t10, observed).
narrative_ontology:measurement(jati_su_t15, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(jati_su_t15, observed).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(jati_su_t20, observed).
narrative_ontology:measurement(jati_su_t25, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(jati_su_t25, observed).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(jati_su_t30, observed).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(jati_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.22).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, brahminical_ritual_authority).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, vedic_occupational_cosmology).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel admits three distinct constraint readings, each with different structural claims about whether jati boundaries are fixed or negotiable. This story (orthodox_textual_reading) asserts boundaries are scriptural and immutable (high extractiveness, snare). The localized_practice_reading asserts boundaries are continuous coordination subject to local drift (lower extractiveness, rope-to-tangled_rope). The colonial_census_reading asserts boundaries were reified by administrative enumeration, not preexistent (moderate extractiveness, tangled_rope). All three readings reference the same kernel (Vedic varna cosmology) but instantiate different ε values and different beneficiary/victim structures. They affect each other: success of one reading undermines the legitimacy of alternatives; reform movements that adopt localized or census readings directly challenge orthodox textual authority. The network links enable constraint-family analysis: the three readings together capture the empirical contest over jati nature across three centuries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
