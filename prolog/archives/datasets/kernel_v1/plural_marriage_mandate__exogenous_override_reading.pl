% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Federal Coercion Against Plural Marriage: The 1890 Manifesto as Exogenous Override
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous_override reading of the
 *   plural_marriage_mandate kernel. The 1890 Manifesto, issued by LDS Church
 *   president Wilford Woodruff, declared the cessation of plural marriage
 *   within the faith. From the exogenous_override reading, this was not
 *   doctrinal reinterpretation but federal coercion forcing the abandonment
 *   of what the reading treats as a permanently divine requirement. The
 *   federal government, through the Morrill Anti-Bigamy Act (1862), Poland
 *   Act (1874), and Edmunds Act (1882), imposed escalating penalties:
 *   imprisonment of polygamists, seizure of church property, disincorporation
 *   of the institution itself. By 1890, the LDS Church faced institutional
 *   dissolution. The Manifesto emerged as a capitulation narrative framed as
 *   prophetic revelation to maintain theological legitimacy while complying
 *   with federal demands. From the exogenous_override perspective, the
 *   'revelation' is theater masking coercive extraction — the federal
 *   government extracts conformity; the church leadership extracts
 *   institutional survival by shifting the suppression burden to practicing
 *   polygamists; practicing polygamists face the full constraint: either
 *   abandon a doctrine they identify as divinely required, or face
 *   imprisonment and property loss. The exogenous_override reading treats the
 *   constraint as a snare — high coercion, minimal coordination benefit,
 *   existence dependent on suppressing the alternative (maintaining plural
 *   marriage practice).
 *
 * KEY AGENTS:
 *   - Practicing Polygamists: Primary victims (powerless/trapped) — face federal imprisonment, property seizure, disenfranchisement; no exit that preserves both theological identity and legal standing
 *   - Ordinary Church Members: Secondary victims (moderate/constrained) — face social ostracism, institutional pressure, relational severance if maintaining pre-1890 doctrinal commitments
 *   - Church Leadership (LDS Institution): Organized beneficiary (organized/arbitrage) — orchestrates Manifesto narrative claiming prophetic revelation while capitulating to federal power; preserves institutional survival and property rights by shifting suppression to membership
 *   - Federal Government: Institutional beneficiary (institutional/arbitrage) — extracts territorial conformity through legal coercion; experiences constraint as law enforcement coordination, not extraction
 *   - Analytical Observer (Exogenous Override Reading): Views the constraint as external political coercion masked as doctrinal development; treats revelation narrative as theater legitimating capitulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Coercion Against Plural Marriage: The 1890 Manifesto as Exogenous Override").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, 'a4a43856-efd0-4ac6-aff9-373b1b7d813c').
narrative_ontology:cs_kernel_codification('a4a43856-efd0-4ac6-aff9-373b1b7d813c', fixed_text).
narrative_ontology:cs_authority_grounding('a4a43856-efd0-4ac6-aff9-373b1b7d813c', extraction).
narrative_ontology:cs_interpretation_layer_present('a4a43856-efd0-4ac6-aff9-373b1b7d813c').
narrative_ontology:cs_reading_relation('a4a43856-efd0-4ac6-aff9-373b1b7d813c', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4a43856-efd0-4ac6-aff9-373b1b7d813c', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('a4a43856-efd0-4ac6-aff9-373b1b7d813c', foundational, divine_requirement_immutable_status).
narrative_ontology:cs_axiom_status(divine_requirement_immutable_status, holdable).
narrative_ontology:cs_axiom_grounding('a4a43856-efd0-4ac6-aff9-373b1b7d813c', divine_requirement_immutable_status, deontological).
narrative_ontology:cs_axiom('a4a43856-efd0-4ac6-aff9-373b1b7d813c', foundational, federal_coercion_primacy).
narrative_ontology:cs_axiom_status(federal_coercion_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a4a43856-efd0-4ac6-aff9-373b1b7d813c', federal_coercion_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('a4a43856-efd0-4ac6-aff9-373b1b7d813c', divine_plural_marriage_requirement).
narrative_ontology:cs_drift_state('a4a43856-efd0-4ac6-aff9-373b1b7d813c', post_1890_manifesto_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('a4a43856-efd0-4ac6-aff9-373b1b7d813c', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, territorial_conformity_apparatus).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, theologically_faithful_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING POLYGAMIST (SNARE) — Faces federal imprisonment, property seizure, and disenfranchisement. The constraint offers no exit: obedience requires abandoning a doctrine identified as divinely required; resistance results in carceral punishment and economic destruction. Maximum extraction with full suppression. The 'voluntary' nature of the Manifesto is performative — assent is extracted under duress.
constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORDINARY CHURCH MEMBER (SNARE) — Faces significant suppression: social ostracism, institutional pressure to affirm the Manifesto, exclusion from community participation if they maintain pre-1890 doctrinal commitments. The constraint operates through shame and relational severance alongside legal penalties on active practitioners. Exit is costly but structurally possible (leave the church, migrate); extraction is high but not maximal — some agency remains.
constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHURCH LEADERSHIP (TANGLED ROPE) — Orchestrates the Manifesto narrative claiming prophetic revelation while capitulating to federal coercion. Genuine coordination function: the leadership navigates between maintaining doctrinal legitimacy and surviving federal assault. Genuine asymmetric extraction: leadership preserves institutional survival and property rights while imposing dissolution of plural marriage on the membership. Leadership experiences mixed extraction and coordination — they extract institutional survival from the constraint while coordinating the community's adaptation to duress.
constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (ROPE) — Perceives the constraint as pure coordination: standardizing territorial religious practice to national law norms. Federal actors genuinely coordinate with state and territorial authorities on uniform legal frameworks. The extraction from the federal perspective is invisible — enforcement of law is experienced as coordination with other governmental units. The federal actor has high arbitrage capacity: they can exit this particular enforcement regime and move resources elsewhere, but they perceive no reason to do so. The constraint appears as justified law enforcement, not extraction.
constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — EXOGENOUS OVERRIDE READING (SNARE) — From this reading's epistemic position, the constraint is external political coercion masquerading as doctrinal development. The 'revelation' narrative is analytical theater: a legitimacy frame applied retroactively to capitulation. The analysis treats the 1890 event as evidence of exogenous override rather than endogenous reinterpretation. High extractiveness, high suppression, moderate theater because the revelation narrative requires maintenance — institutional actors must continuously perform the legitimacy frame.
constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plural_marriage_mandate__exogenous_override_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The federal government extracts conformity through carceral threat and property seizure. The church leadership extracts institutional survival by making rank-and-file members bear the cost of compliance. The overall extraction is severe but not absolute (Snare floor ≥ 0.66) because some practitioners retain capacity to migrate, hide, or resist. The extractiveness measurement shows upward trend (0.58 → 0.72 → 0.68) reflecting escalating federal enforcement from 1860s through the 1890s Manifesto era, then stabilization as the suppression machinery matured and the church's capitulation became institutionalized. Suppression (0.82): Very high. The constraint operates through multiple channels: federal criminal law (imprisonment), property seizure, institutional coercion (church pressure to affirm the Manifesto), social ostracism, relational severance within the community. The suppression floor for a Snare is 0.60; 0.82 indicates near-total elimination of practical alternatives — practitioners can theoretically resist, but the cost is carceral + economic + relational. The measurement shows upward trajectory (0.75 → 0.85 → 0.82) as federal enforcement escalated and institutional machinery synchronized with federal objectives. Theater ratio (0.65): Moderate-high. The Manifesto's revelation narrative is the primary theatrical element — the claim that God revealed the temporal suspension of plural marriage requires continuous institutional maintenance. The theater is necessary because the institutional legitimacy frame would collapse if practitioners accepted the naked fact of federal coercion. However, the theater is not total (piton floor ≥ 0.70) because federal law enforcement operates openly and undeniably; the coercion is transparent even as the theological justification is performed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces sharp perspectival divergence from the same structural facts. The practicing polygamist sees a snare: they are asked to choose between theological identity and bodily freedom, with no compromise available. The ordinary church member sees a constrained snare: they benefit from institutional survival (the church continues to exist) but are pressured to repudiate pre-1890 doctrine (the cost is partly relational/psychological, partly legal for those actively practicing). The church leadership sees a tangled rope: they coordinate institutional survival (genuine coordination problem: how does the institution survive federal assault?) while extracting the doctrinal shift from the membership (asymmetric extraction: leadership preserves institutional property and authority; members lose doctrinal coherence). The federal government sees a rope: law enforcement coordinating with state and territorial authorities, standardizing religious practice to national norms. The analytical observer (exogenous_override reading) sees a snare: external political coercion masked as doctrinal revelation. The perspectival gap reveals that the same constraint is experienced as snare/rope/tangled_rope depending on the agent's structural position — beneficiaries perceive coordination; targets perceive extraction; institutional mediators perceive both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) follow from the structural relationship of each agent to the constraint. Practicing polygamists: d ≈ 0.95 (full victims, trapped, maximum d). Ordinary church members: d ≈ 0.70 (mixed — they bear costs of institutional suppression but benefit from survival; exit is constrained but possible). Church leadership: d ≈ 0.20 (beneficiaries, arbitrage exit, low d). Federal government: d ≈ 0.05 (full beneficiaries in terms of conformity extraction; arbitrage exit because they can reallocate enforcement resources). The exogenous_override reading emphasizes that the beneficiary set is the federal government and the institutional apparatus, not endogenous theological agents — the reading's core claim is that the extraction flow is exogenous (imposed from outside) rather than endogenous (emerging from doctrinal reinterpretation). The schema's derived d values support this interpretation: victims have high d (trapped, no exit); beneficiaries have low d (arbitrage, institutional power).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has extractiveness > 0.70, requiring mandatrophy resolution. The mandatrophy (the apparent paradox that a constraint is both coordination and extraction) is resolved by the indexed perspectives: the constraint genuinely coordinates from the federal/leadership institutional perspectives (they solve real coordination problems: law enforcement standardization, institutional survival) while genuinely extracting from the victim perspective (the practicing polygamists and ordinary members have their agency and identity constrained). The exogenous_override reading resolves mandatrophy by arguing that the 'coordination' is imposed from outside — federal coercion creating a coordination problem that the church then solves by capitulating. The extraction is primary; the coordination is secondary and instrumental to extraction. The revelation narrative (theater) bridges the mandate paradox by framing extraction as doctrinal development, making compliance seem like internal reinterpretation rather than external coercion. This is precisely what the exogenous_override reading contests: it treats the theater as theater, and the underlying extraction as primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_revelation_versus_coercive_capitulation,
    'Does the 1890 Manifesto represent authentic prophetic revelation (endogenous reinterpretation) or federal coercion masked as revelation narrative (exogenous override)?',
    'Historical analysis of internal church deliberations, comparison of linguistic/theological markers with documented revelation claims, chronological correlation with federal legal escalation (Morrill Anti-Bigamy Act 1862, Poland Act 1874, Edmunds Act 1882), examination of leadership statements contemporaneous to drafting vs. post-hoc theological justification.',
    'If authentic revelation: constraint is tangled_rope (genuine coordination with embedded extraction). If coercive capitulation: constraint is snare (pure extraction with theater). If institutional pragmatism (sibling reading): constraint is tangled_rope but with different axioms and authority grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_revelation_versus_coercive_capitulation, conceptual, 'Whether 1890 Manifesto is prophetic revelation or coercive capitulation').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (exogenous_override_reading) of the contested kernel (plural_marriage_mandate). Which reading is instantiated here, and what are the sibling readings?',
    'Semantic documentation in cs_structure.reading_relations and cs_structure.axioms. This omega records the committer-frame classification: the same 1890 Manifesto event admits three distinct structural interpretations held by different epistemic communities.',
    'The engine routes this story to the plural_marriage_mandate family. Sibling reading stories (endogenous_reinterpretation_reading, institutional_pragmatism_reading) model the same event under different axioms. Network linkage via constraint family enables cross-reading analysis — no single reading is privileged; the contest itself is the structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identity of this constraint as exogenous_override_reading within plural_marriage_mandate kernel').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.82) primarily structural (external legal penalties, property seizure, imprisonment) or internalized (theological reframing that makes practitioners accept the constraint as legitimate)?',
    'Post-1890 trajectory of practicing polygamists: suppression persistence after legal penalties ceased (did suppression decline when federal coercion relaxed? did internalized frames remain?). Church membership surveys documenting belief shifts. Historical narratives of practitioners who resisted vs. capitulated and the ex-ante vs. ex-post belief positions.',
    'If primarily structural: suppression declines sharply after 1910s when federal enforcement eased. If partly internalized: suppression persists even as external barriers relaxed — the constraint''s binding mechanism shifted from external coercion to internalized theological reframing. Internalization shifts the classification toward identity_locked exit for later generations, even as the original constraint operated via trapped exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural external penalties or internalized theological legitimation').

omega_variable(
    doctrinal_continuity_versus_rupture,
    'From the exogenous_override perspective, can the 1890 Manifesto be framed as continuous doctrinal development, or does it represent irreconcilable rupture with pre-1890 theology?',
    'Textual analysis: does pre-1890 doctrine explicitly identify plural marriage as permanently required, or only temporally required? Are there doctrinal resources within pre-1890 texts that admit suspension? Comparative analysis: did other theologically similar traditions (FLDS, fundamentalist branches) classify the Manifesto as rupture or reinterpretation?',
    'If continuous development: the exogenous_override reading is weakened — theological resources for reinterpretation existed endogenously. If irreconcilable rupture: the exogenous_override reading is strengthened — the break is too sharp to attribute to doctrinal evolution. This shapes the role and status of the axiom divine_requirement_immutable_status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_continuity_versus_rupture, empirical, 'Whether 1890 Manifesto represents continuous doctrinal development or rupture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_exo_theater_pre_manifesto, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(plur_exo_theater_manifesto_era, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement(plur_exo_theater_sustained, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(plur_exo_extract_pre_manifesto, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(plur_exo_extract_early_enforcement, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(plur_exo_extract_mature_enforcement, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_exo_suppress_pre_manifesto, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(plur_exo_suppress_peak_enforcement, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(plur_exo_suppress_sustained, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel admits three structurally distinct constraint stories representing different readings of the same 1890 Manifesto event. This story (exogenous_override_reading) treats the Manifesto as federal coercion forcing abandonment of a divinely required practice, classified as a snare. The sibling readings (endogenous_reinterpretation, institutional_pragmatism) model the same event under different axioms and authority groundings. All three stories share the same kernel_id (plural_marriage_mandate) and populate cs_structure.reading_relations to encode the structural relationships between readings: this reading coexists_with the endogenous_reinterpretation reading (neither forecloses the other — they are held by different epistemic communities) and influences the institutional_pragmatism reading (the exogenous facts of federal coercion create structural conditions that the pragmatism reading acknowledges).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
