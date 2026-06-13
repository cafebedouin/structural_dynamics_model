% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Coercion Against Plural Marriage (Exogenous Override Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   Between 1862 and 1890, the federal government deployed military
 *   occupation, property seizure, imprisonment, and disenfranchisement to
 *   coerce the Latter-day Saint Church and its members to abandon plural
 *   marriage. The 1890 Manifesto, authored under duress, is framed by the
 *   exogenous-override reading as a surrender document: the church leadership
 *   repackaged coerced capitulation as a divine revelation to preserve
 *   institutional survival. This reading asserts that the federal suppression
 *   is an extraction mechanism (snare) masked by a theological narrative of
 *   voluntary reinterpretation. The expected structural delta is high
 *   suppression (coercive force required to enforce the abandonment) and
 *   clear victim/beneficiary asymmetry (federal government benefits from
 *   territorial conformity; practicing polygamists and the church bear the
 *   costs of dispossession and forced doctrinal revision). The measurement
 *   series documents extraction and suppression intensification over the
 *   interval, with theater ratio rising sharply as the enforcement machinery
 *   reached its peak (1887 onwards) and the church's reinterpretation
 *   narrative became the primary mode of compliance achievement post-1890.
 *
 * KEY AGENTS:
 *   - Federal government: institutional agenda-setter, wielding military and legal coercion; benefits from territorial conformity and settler-colonial integration
 *   - Practicing polygamists: powerless victims, trapped between imprisonment/dispossession and abandonment of religiously mandated practice
 *   - Latter-day Saint Church institutional leadership: organized agent under duress, choosing organizational survival via reinterpretation at the cost of members' spiritual/relational autonomy
 *   - Federal territorial authorities: institutional agenda-setter on the ground, executing military occupation and property seizure
 *   - Monogamous American settlers: beneficiaries of suppression, gain social/legal conformity without bearing enforcement costs
 *   - Non-practicing church members: excluded, disenfranchised despite not practicing plural marriage, absent from federal deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.87).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.91).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Coercion Against Plural Marriage (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '0a0c9208-7612-4845-90e5-2353cfcc9650').
narrative_ontology:cs_kernel_codification('0a0c9208-7612-4845-90e5-2353cfcc9650', fixed_text).
narrative_ontology:cs_authority_grounding('0a0c9208-7612-4845-90e5-2353cfcc9650', extraction).
narrative_ontology:cs_interpretation_layer_present('0a0c9208-7612-4845-90e5-2353cfcc9650').
narrative_ontology:cs_reading_relation('0a0c9208-7612-4845-90e5-2353cfcc9650', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('0a0c9208-7612-4845-90e5-2353cfcc9650', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('0a0c9208-7612-4845-90e5-2353cfcc9650', foundational, plural_marriage_divinely_mandated).
narrative_ontology:cs_axiom_status(plural_marriage_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('0a0c9208-7612-4845-90e5-2353cfcc9650', plural_marriage_divinely_mandated, deontological).
narrative_ontology:cs_axiom('0a0c9208-7612-4845-90e5-2353cfcc9650', foundational, federal_authority_cannot_override_divine_mandate).
narrative_ontology:cs_axiom_status(federal_authority_cannot_override_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('0a0c9208-7612-4845-90e5-2353cfcc9650', federal_authority_cannot_override_divine_mandate, deontological).
narrative_ontology:cs_reference_frame('0a0c9208-7612-4845-90e5-2353cfcc9650', prophetic_authority_over_family_practice).
narrative_ontology:cs_drift_state('0a0c9208-7612-4845-90e5-2353cfcc9650', post_1890_federal_coercion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0a0c9208-7612-4845-90e5-2353cfcc9650', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_church).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_church).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, monogamous_american_settlers).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_territorial_sovereignty_over_religious_practice).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, monogamy_as_civilizational_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces monogamy as a requirement for Utah statehood and territorial governance. Uses military occupation, seizure of church property, imprisonment of practicing polygamists, and disenfranchisement of church members to coerce abandonment. Frames coercion as enforcement of civilizational norms and federal sovereignty over territory.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Face imprisonment, property confiscation, loss of voting rights, and family separation if they continue plural marriage. Alternatives are: renounce a central religious practice they believe divinely mandated, leave the territory (but the federal government controls entry/settlement/property sale), or persist in practice and accept incarceration and dispossession. The cost of resistance is catastrophic and unequally distributed among families.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, trapped, regional).

% Institutional leadership faces a choice: defend plural marriage doctrine and lose the organization to property seizure and its members to imprisonment, or reframe doctrine to declare the coerced abandonment a revelation. The 1890 Manifesto is authored by church leadership under duress. Leadership benefits from organizational survival but members bear the spiritual and relational costs of the reinterpretation. The church's exit options are nominal (it can leave the territory but abandons its institutional base; it can resist but faces institutional dissolution).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_church, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_church, beneficiary).

% Benefit from the constraint's operation: plural marriage is suppressed, reducing social friction around marriage norms and enabling settler colonialism in Utah on terms familiar from the Eastern U.S. They do not bear direct enforcement costs; those are transferred to federal enforcement machinery and to practicing polygamists.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, monogamous_american_settlers, beneficiary,
    organized, generational, mobile, national).

% Execute federal coercion on the ground: military officers, appointed territorial governors, federal marshals. They carry out enforcement that imprisonment and property seizure require, and they administer the mechanism that makes continued resistance materially impossible.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_territorial_authorities, agenda_setter,
    institutional, biographical, analytical, regional).

% Are subject to disenfranchisement and harassment regardless of their personal practice status because of church membership. They lose political voice and property rights while not directly resisting the constraint. Their objections to the reinterpretation narrative are structurally absent from federal deliberation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, non_practicing_church_members, excluded,
    moderate, biographical, constrained, regional).

% Provides ideological framing that positions monogamy as civilizational and plural marriage as primitive/uncivilized, legitimizing federal intervention as enforcing progress. This observer position carries no direct enforcement cost but shapes the narrative terrain that makes coercion legible as law rather than oppression.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, mainstream_protestant_american_discourse, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__exogenous_override_reading, mainstream_protestant_american_discourse).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None: the constraint does not solve a genuine coordination problem among willing participants. Federal enforcement of monogamy is unilateral coercion of a minority practice, not coordination of a collective-action problem.
% TRANSFER_FUNCTION: Transfers religious practice authority from the Latter-day Saint Church to the federal government. Transfers property from church and individual polygamists to the federal government via seizure. Transfers reproductive/family-formation autonomy from practicing members to federal law. Transfers political voice from all church members (via disenfranchisement) to federal-aligned settlers.
% ABSENT_VOICES: Practicing polygamists are not at the federal table where monogamy is declared a condition of statehood; they only appear as objects of enforcement, not negotiators. Non-practicing church members are disenfranchised and their stake in the reinterpretation (forced spiritual acceptance) is not solicited. Native peoples and other religious minorities who might object to federal enforcement of a majority Christian sexual ethic are similarly absent from deliberation.
% DISAPPEARANCE_RATIONALE: If federal coercion and the 1890 Manifesto reinterpretation both vanished, plural marriage would persist among those who practice it, the church would retain property and its members would retain political voice and family autonomy. The federal government would lose a key mechanism for territorial conformity and settler-colonial integration. The constraint's disappearance reorganizes power: the church regains institutional autonomy, practicing members regain family structure, the government loses enforcement leverage.
% FOUNDING_PROBLEM: Federal government sought to suppress plural marriage as incompatible with U.S. territorial expansion and settler-colonial governance norms. The founding problem is stated from the federal perspective: plural marriage was an obstacle to integrating Utah into the Union on terms of American monogamous family law.
% FOUNDING_PROBLEM_CORROBORATION: Congressional testimony, federal territorial legislation (Morrill Anti-Bigamy Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887), and correspondence from territorial governors document the federal founding problem: plural marriage was an impediment to statehood and federal control. The church itself, under coercion, attests to the problem in the 1890 Manifesto framing (a coerced attestation, not an independent corroboration). Independent historical scholarship from outside federal and church benefiting parties confirms federal coercion was the driver and the Manifesto was a surrender document, not an authentic doctrinal reinterpretation.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.65→0.87 across the interval) because the federal government systematically transfers property, political voice, and reproductive autonomy from the church and its members. The measurement captures federal legislative intensification: Morrill Act (1862, initial framework), Edmunds Act (1882, property seizure and disenfranchisement), Edmunds-Tucker Act (1887, institutional targeting). Suppression is higher still (0.91 at 1890) because coerced abandonment requires continuous enforcement machinery—military presence, marshals, courts, property administrators—not voluntary compliance. Theater ratio rises sharply (0.22→0.68) as the constraint nears its endpoint: early suppression is raw (explicit legal prohibition, visible enforcement). By 1887-1890, as resistance becomes materially impossible, the 1890 Manifesto reinterpretation narrative becomes the primary mode of compliance—theater replaces naked force as the visible mechanism. The post-1890 plateau reflects stabilization: suppression remains high (enforcement machinery persists to ensure compliance), theater remains elevated (the reinterpretation narrative requires continuous ritual maintenance—church leadership reaffirming the revelation, members performing acceptance), extraction plateaus (the primary transfers are complete; rents from ongoing suppression are lower than the initial seizure phase).
 *
 * PERSPECTIVAL GAP:
 *   The federal agenda-setter seat and the victim seat should compute radically differently. From the federal perspective, this is law enforcement (legitimate suppression of a practice incompatible with U.S. governance). From the practicing polygamist seat, this is targeted extraction justified by a coerced theological narrative. From the church leadership seat, this is institutional self-preservation at the cost of member autonomy. The engine computes these divergences from stakeholder power, exit, and the beneficiary/victim declarations—the authored claim (snare) explicitly asserts the constraint is extraction, not legitimate law, a disagreement with federal self-interpretation that the computational phase should capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: d ≈ 0.1 (beneficiary, high institutional power, arbitrage exit—can move enforcement elsewhere, competes within its own institutional framework, not trapped). Practicing polygamists: d ≈ 0.95 (victims, powerless, trapped exit—physical territory is occupied, property is controlled, alternatives are foreclosed by federal reach). Church institutional leadership: d ≈ 0.70 (payer in the extraction sense—forced capitulation at cost of member autonomy; secondary beneficiary role recognizes organizational survival, but the extraction logic dominates because the church is not a willing participant in the suppression and retains no control over its terms). Monogamous settlers: d ≈ 0.2 (beneficiary, organized power, mobile exit—benefit from suppression without direct enforcement cost). The high directionality asymmetry (0.1 for beneficiary vs 0.95 for victims) drives high computed extraction from the victim perspective, even though the base extractiveness is measured as a constraint property. No overrides needed; derivation from beneficiary/victim + exit options produces the correct structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy: its founding problem (federal suppression of plural marriage as an impediment to territorial conformity) remains live and its persistence depends on continuous enforcement to prevent reversion to plural practice. The theater ratio rise is not a symptom of function atrophy—it is a symptom of enforcement strategy shift, from raw coercion to narrative maintenance. The constraint persists because its founding problem persists (federal governance norms require monogamy) and because the coercive machinery remains active (church leadership continues to enforce the reinterpretation against internal resistance). If anything, mandatrophy might apply to the 1890 Manifesto as a *narrative device*: its initial function was to render coercion legible as revelation, papering over the extraction. Over time, the narrative becomes the primary maintenance mode because the coercive apparatus is so thoroughly internalized (church members police plural practice among themselves in the reinterpretation's name). But the underlying extraction—the federal appropriation of religious authority—persists and would revert to raw coercion if the narrative failed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_ambiguity,
    'Is the 1890 Manifesto a genuine prophetic revelation of divine will, or a coerced capitulation narrative authored to make federal suppression legible as spiritual obedience?',
    'Textual analysis of 1890 Manifesto composition (authorship, editing, timing relative to enforcement escalation); interview/testimony evidence from church leadership about the authoring process (available in later historical records, oral histories); comparison of the Manifesto''s theological claims to the church''s prior doctrinal framework (did revelation claims align with established prophetic procedures, or break from them?); analysis of whether the revelation narrative was adopted pre-emptively (church chose reinterpretation) or reactively (reinterpretation followed enforcement escalation).',
    'If the Manifesto is revealed to be a coerced document authored under duress with a manufactured revelation narrative, the exogenous-override reading is strengthened and the constraint type remains snare. If the Manifesto is revealed to be a genuine revelation process consistent with prior church practice, the endogenous-reinterpretation reading gains weight and the constraint type should be reclassified. If evidence shows pragmatic institutional judgment presented as revelation, the institutional-pragmatism reading advances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_ambiguity, empirical, 'The epistemic status of the 1890 Manifesto''s revelation claim.').

omega_variable(
    coercion_suppression_mechanism,
    'Is the measured suppression (0.91) structural (external barriers: imprisonment, property seizure, military occupation) or internalized (the victims accept the reinterpretation as legitimate and police themselves)?',
    'Post-1890 resistance trajectory: if plural marriage persists in secret practice and enforcement machinery must intensify to suppress it, suppression is structural (external barriers remain necessary). If plural marriage is abandoned and the reinterpretation is accepted as theologically valid by the membership, suppression has become substantially internalized. Archival evidence of internal church resistance vs. acceptance post-1890; demographic data on plural marriage prevalence after the Manifesto (if it declines rapidly and stays low, internalization is supported; if it persists and enforcement escalates, suppression remains structural).',
    'If suppression is structural, the snare classification holds: coercive extraction persists and requires continuous machinery. If suppression is internalized, the theater ratio increases and the constraint evolves toward piton status (enforcement becomes performative because the norm is accepted). The exogenous-override reading''s core claim—that this is coercion, not reinterpretation—depends on suppression remaining structural; internalization would partially vindicate the endogenous-reinterpretation reading''s claim that the revelation was accepted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_suppression_mechanism, empirical, 'Whether suppression is structural or internalized; post-1890 resistance trajectory.').

omega_variable(
    federal_benefit_extraction_vs_civilizational_norm_enforcement,
    'Is the federal government extracting concrete benefits (territorial conformity, property seizure, political control) from the suppression, or is it enforcing a genuinely held civilizational norm that would be enforced regardless of federal benefit?',
    'Comparative analysis: did the federal government apply similar coercive enforcement to other religious practices that deviated from monogamy (e.g., Muslim immigration, religious communes experimenting with alternative kinship)? Were those practices suppressed with the same machinery? If monogamy enforcement was selective (applied forcefully to the church, leniently to others where federal benefit was lower), the extraction motive is revealed. If enforcement was consistent across all deviant practices, the norm-enforcement framing is supported.',
    'If enforcement is selective and benefit-driven, the snare classification is strengthened (this is coercion for federal gain). If enforcement is consistent, the classification remains snare but the narrative reframes to ''universal norm enforcement via selective coercion''—still extractive but motivated by civilizational ideology, not pure rent-seeking. The exogenous-override reading allows for benefit-extraction as the driver; the endogenous reading would position norm-enforcement as the sole driver.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_benefit_extraction_vs_civilizational_norm_enforcement, empirical, 'Whether federal suppression was benefit-extraction or civilizational norm enforcement.').

omega_variable(
    kernel_reading_frame_contest,
    'Which kernel reading—exogenous override, endogenous reinterpretation, or institutional pragmatism—most accurately captures the church leadership''s own epistemic stance during the authoring of the 1890 Manifesto?',
    'Archival evidence from church leadership correspondence, private diaries, and retrospective testimony about whether they believed (A) federal coercion forced them to abandon a divine requirement (exogenous), (B) God revealed the temporal suspension via prophetic authority (endogenous), or (C) they made a pragmatic institutional judgment and presented it as revelation (pragmatism). The most direct evidence would be leadership correspondence written before the Manifesto''s public release, discussing the decision and its reasoning.',
    'This is a conceptual/preference question about reading authority: which reading does the kernel''s own steward endorse? If leadership believed exogenous, the reading is corroborated internally. If they believed endogenous or pragmatism, the exogenous reading is an external observer''s contestation. This does not change the constraint''s measured properties (extraction, suppression, theater remain authored as observed), but it affects which reading is positioned as primary vs. contestatory. The engine does not compute reading authority; this omega documents the ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_frame_contest, conceptual, 'Which kernel reading the constraint''s own authority claimed, and whether that claim is corroborated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1896).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.22).
narrative_ontology:measurement_basis(plur_tr_t1862, observed).
narrative_ontology:measurement(plur_tr_t1870, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1870, 0.31).
narrative_ontology:measurement_basis(plur_tr_t1870, observed).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.45).
narrative_ontology:measurement_basis(plur_tr_t1882, observed).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.58).
narrative_ontology:measurement_basis(plur_tr_t1887, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.68).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.68).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.65).
narrative_ontology:measurement_basis(plur_be_t1862, observed).
narrative_ontology:measurement(plur_be_t1870, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1870, 0.72).
narrative_ontology:measurement_basis(plur_be_t1870, observed).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.81).
narrative_ontology:measurement_basis(plur_be_t1882, observed).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.85).
narrative_ontology:measurement_basis(plur_be_t1887, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.87).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.87).
narrative_ontology:measurement_basis(plur_be_t1896, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.58).
narrative_ontology:measurement_basis(plur_su_t1862, observed).
narrative_ontology:measurement(plur_su_t1870, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1870, 0.68).
narrative_ontology:measurement_basis(plur_su_t1870, observed).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.79).
narrative_ontology:measurement_basis(plur_su_t1882, observed).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.88).
narrative_ontology:measurement_basis(plur_su_t1887, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.91).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.91).
narrative_ontology:measurement_basis(plur_su_t1896, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__exogenous_override_reading, 0.15).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the 1890 Manifesto's epistemic status. This constraint (exogenous-override reading) asserts the Manifesto was coerced capitulation to federal suppression. The endogenous-reinterpretation reading asserts the Manifesto was a genuine prophetic revelation of God's will. The institutional-pragmatism reading asserts the Manifesto was pragmatic institutional judgment presented via revelation narrative. All three share the same historical event (the 1890 Manifesto) but differ fundamentally in their ε values: exogenous-override assigns high extractiveness (coercion), endogenous-reinterpretation assigns low extractiveness (legitimate doctrinal evolution), institutional-pragmatism assigns moderate-to-high extractiveness (strategic adaptation). The readings' ε-invariance principle requires separate stories because the observable (the Manifesto text and event) does not determine extraction—the reading's interpretation of the Manifesto's authority does. Each reading produces a different constraint type and different victim/beneficiary structure. They are linked by this network edge so contamination analysis can track how the kernel reading contest affects institutional purity and coupling in the broader religious governance system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
