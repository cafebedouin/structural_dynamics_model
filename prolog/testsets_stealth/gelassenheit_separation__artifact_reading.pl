% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Visible-Distinction Resemblance Test (Artifact Reading of Gelassenheit Separation)
 *   domain: religious/technological/commitment-systems
 *
 * SUMMARY:
 *   Under the artifact reading of the gelassenheit_separation kernel,
 *   separation from English society consists in visible distinction, and the
 *   operative test for any technology is resemblance to a worldly artifact
 *   regardless of function. Off-grid solar is forbidden because a
 *   photovoltaic panel looks like an English artifact even though it
 *   entangles nothing; modern fabrics are forbidden though functionally
 *   identical to approved cloth. The regime is administered by district
 *   bishops through the Ordnung, enforced by confession, ban, and shunning.
 *   Epsilon's referent is the standing arrangement under contest - the
 *   visible-marker regime with resemblance testing as actually operated -
 *   assessed against what any account of separation requires; the sibling
 *   readings are separate constraints, linked via
 *   network.affects_constraints, not folded into this one. KEY AGENTS (by
 *   structural relationship): district_bishops_ministry: agenda setter and
 *   primary beneficiary (institutional/identity_locked) - administers the
 *   resemblance line and collects interpretive authority; baptized_members:
 *   primary target (powerless/trapped) - bear compliance costs divorced from
 *   separation function; rumspringa_youth: excluded pre-commitment seat
 *   (moderate/mobile) - face the boundary without a voice in setting it;
 *   plain_goods_merchants: secondary beneficiary (organized/mobile) - profit
 *   from the marker economy; technology_vendors: excluded supplier seat
 *   (powerful/arbitrage) - barred regardless of product function;
 *   anabaptist_religious_scholars: analytical observer - sees the full
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.72).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.85).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Visible-Distinction Resemblance Test (Artifact Reading of Gelassenheit Separation)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technological/commitment-systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0').
narrative_ontology:cs_kernel_codification('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', fixed_text).
narrative_ontology:cs_authority_grounding('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', lineage).
narrative_ontology:cs_interpretation_layer_present('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0').
narrative_ontology:cs_reading_relation('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', foundational, separation_consists_in_visible_distinction).
narrative_ontology:cs_axiom_status(separation_consists_in_visible_distinction, holdable).
narrative_ontology:cs_axiom_grounding('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', separation_consists_in_visible_distinction, deontological).
narrative_ontology:cs_axiom('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', foundational, resemblance_test_trumps_function).
narrative_ontology:cs_axiom_status(resemblance_test_trumps_function, holdable).
narrative_ontology:cs_axiom_grounding('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', resemblance_test_trumps_function, conventional).
narrative_ontology:cs_reference_frame('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', visible_marker_nonconformity).
narrative_ontology:cs_drift_state('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', contemporary_technology_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd6a0d94-8cfe-404f-a501-1b96b3aaf2e0', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, district_bishops_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, plain_goods_merchants).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, baptized_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, rumspringa_youth).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visible_marker_separation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and ministers of conservative districts administer the Ordnung and rule on whether a proposed artifact 'looks English'. Every resemblance ambiguity resolves to their judgment, and the formalist criterion maximizes the number of judgments required. They live under the same dress and transport rules they enforce, and their standing in the community is constituted by the interpreter role; abandoning the tradition ends their office, their kin standing, and their perceived salvation at once.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, district_bishops_ministry, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, district_bishops_ministry, beneficiary).

% Forgo off-grid solar for shop refrigeration and home lighting even where no grid tie exists, wear prescribed fabrics, and accept labor-intensive substitutes. Each renunciation is individually small; together they compose a life-shape. Dissent runs through confession, ban, and shunning, which sever business ties with family firms. Leaving means losing the kin network, the livelihood, and, in their formed understanding, salvation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, baptized_members, payer,
    powerless, biographical, trapped, regional).

% Teenagers before baptism sample English life and later face the baptism choice. They have no seat in the Ordnung deliberations that define what they must forswear. Pre-baptism they may leave without shunning, but family, formation, and the absence of any outside competence pull most back before the choice matures.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, rumspringa_youth, excluded,
    moderate, immediate, mobile, regional).

% Sell plain coats, broadfall trousers, buggy hardware, kerosene lamps, and off-grid appliances styled to pass the resemblance test. The marker economy is their customer base, and they retool product styling whenever a ruling moves the line. They collect revenue from the constraint without running it.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, plain_goods_merchants, beneficiary,
    organized, biographical, mobile, national).

% Solar installers and textile mills could supply members at competitive rates, but the resemblance rule bars products that look English regardless of function. They serve other markets and treat the community as a closed segment; their loss is foregone revenue, not compulsion, and nothing in the declaration surface records it.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, technology_vendors, excluded,
    powerful, biographical, arbitrage, national).

% Document the Ordnung's evolution, the nineteenth-century Old Order consolidations, and district divergence over solar and telephones. Compare readings of separation across Amish and Mennonite bodies. Hold no stake in any district's outcome.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, anabaptist_religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, district_bishops_ministry).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Visible markers coordinate group identity and mutual recognition across scattered settlements: plain dress, horse-drawn transport, and prescribed household technology make membership legible at a glance. Forgoing convenient technology operates as costly signaling that credibly demonstrates commitment, sustaining the trust that mutual aid, barn raisings, and uninsured risk pooling rest on.
% TRANSFER_FUNCTION: Moves compliance costs from baptized members to the boundary system: foregone technology, prescribed consumption, and labor-intensive practices flow from members, while interpretive authority and deference flow to the ministry, and marker-economy revenue flows to compliant merchants.
% ABSENT_VOICES: Members who evaluate technology by function or by consequence - the positions instantiated in the sibling readings - have no standing under the resemblance test: their arguments are ruled out of order by construction, since the test admits only evidence of visual similarity. Former members who left over technology disputes, and youth approaching baptism, likewise stand outside Ordnung deliberations.
% DISAPPEARANCE_RATIONALE: Districts would diverge immediately: some adopting off-grid solar and mail-order fabrics within a season, others holding the line. Cross-settlement marker consistency would fragment, the ministry's gatekeeping role would shrink to genuine entanglement questions, and merchant product lines would restyle. Worship and mutual aid would continue largely unchanged, since they never depended on the resemblance test specifically - but the arrangement as it stands would reorganize.
% FOUNDING_PROBLEM: After the persecution era, Anabaptist communities needed a workable test for nonconformity to the world as English society industrialized. Case-by-case discernment of each technology's spiritual effect proved unmanageable across scattered settlements with a lay ministry, and a visible test - does it resemble the world's artifact? - offered bishops an administrable line.
% FOUNDING_PROBLEM_CORROBORATION: Anabaptist historians corroborate the founding problem from outside the benefiting parties: the post-1860s Old Order consolidations are documented as responses to industrialization pressure. Neighboring Mennonite bodies, sharing the genealogy but rejecting the formalist test, attest the problem was real while disputing this solution. No source outside the beneficiary set attests that resemblance specifically, rather than function or consequence, is the required test.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the resemblance test imposes costs with no separation-relevant function: an off-grid panel extracts compliance while coordinating nothing that a functional test would not coordinate more cheaply. Suppression is higher still (0.85) and is authored as a raw structural property, unscaled by power or scope: persistence depends on actively suppressing the rival evaluative criteria (functional and consequential arguments are out of order by construction) and on shunning machinery against adopters. Theater crosses 0.5 by interval end because a growing share of enforcement activity defends appearance rather than substantive separation outcomes - resemblance rulings, styling disputes, cosmetic-compliance cases. Accessibility_collapse is moderate-high (0.62): inside the community the alternative collapses completely once the rule is understood (the function cannot be had without the forbidden form), while external alternatives exist only at catastrophic social cost. Resistance is moderate (0.42): petitions for solar, district splits, quiet adoption of borderline items, youth defection. The claim and the metrics are independent authored facts: tangled_rope is claimed because genuine coordination (cross-settlement recognition, credible commitment, mutual-aid trust) demonstrably coexists with extraction exceeding any coordination floor; the engine computes per-seat types from the structural data and measures any divergence. All three tracked series run on one shared time grid (points 0-50, indexing decades from roughly 1965), so every metric is authored at every examined time point; the rising suppression_requirement series is authored deliberately because this story traces enforcement intensification - Ordnung codification and harder discipline as drift pressure grew - not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry seat the arrangement is the tradition itself: administering the resemblance line is what being a bishop is, and identity-lock prevents the seat from pricing its own capture. From the baptized-member seat the same structure is a stream of concrete renunciations - the refrigerator, the fabric, the panel - each small, collectively a life-shape. Plain-goods merchants experience the test as a product-specification environment to be restyled around; technology vendors experience it as a closed door with no recourse. Rumspringa youth experience the boundary as a future choice they did not deliberate. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The ministry sits near the beneficiary pole: the resemblance test routes every adoption question through their ruling, and the formalist criterion maximizes the set of questions requiring judgment - a functional test would be largely self-administering. Baptized members sit near the target pole, amplified by trapped exit. Merchants collect marker-economy revenue with mobile exit, damping their d. Youth are victims-in-waiting whose mobile pre-baptism position moderates their d. Technology vendors are harmed by exclusion yet hold arbitrage elsewhere; the derivation chain cannot see foregone-market harm because vendors appear in neither the beneficiary nor the victim arrays, so an explicit override at the powerful atom (the only powerful seat in this story, so the override binds cleanly) sets d at 0.72. Scholars are analytical and directionally neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare prevents mislabeling: the visible-marker system carries real coordination load - cross-settlement recognition, credible commitment signaling, mutual-aid trust - that would not survive treating the whole structure as pure extraction. Classifying it as anything softer than tangled_rope would miss that the resemblance test extracts compliance without coordinating anything the sibling criteria would not coordinate more cheaply. On the R5 interview, the founding problem (an administrable nonconformity test amid proliferating worldliness) is contested: the problem of worldliness persists, but whether resemblance is the right test is precisely the live dispute among the three readings. The mismatch consumer watches founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement's persistence depends on parties who dispute its criterion - a capture/zombie-risk signature if the contest resolves toward the siblings. Coalition note: baptized members are individually powerless, but the historical record shows collective action works - districts have split and reformed over technology questions, which is why resistance is authored at 0.42 rather than near zero despite the powerless atom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the artifact_reading of the gelassenheit_separation kernel. What would change structurally if a sibling reading governed instead?',
    'Cross-district comparison: districts operating principle-style or consequence-style criteria permit off-grid solar and modern fabrics while retaining mutual aid; measure retention, trust, and boundary stability against artifact-reading districts.',
    'Sibling governance would shrink the victim set (off-grid solar permitted), cut epsilon substantially, reduce ministerial discretion capture, and likely shift the computed type toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the separation kernel governs determines the constraint''s entire victim and extraction structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning, economic interdependence, geographic closure) or internalized (Gelassenheit self-surrender making compliance experienced as piety rather than coercion)?',
    'Post-exit trajectory of leavers: if compliance impulses and guilt persist after the enforcement mechanism is out of reach, reclassify as partially internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure - targets carry the constraint with them after exit, and the trapped-exit coding understates lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in Ordnung enforcement.').

omega_variable(
    costly_signaling_vs_theater,
    'Do the function-divorced prohibitions (off-grid solar, modern fabrics) produce genuine costly-signaling coordination value, or are they enforcement theater maintaining ministerial discretion?',
    'Compare districts that admit off-grid solar: if mutual-aid participation, retention, and cross-district trust hold, the signaling value does not depend on the resemblance bans.',
    'If signaling survives without the bans, the theater share rises and the constraint drifts toward piton or snare; if it collapses, the bans carry real coordination weight and the tangled_rope reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(costly_signaling_vs_theater, empirical, 'Whether resemblance bans buy signaling value or merely defend interpretive discretion.').

omega_variable(
    ministerial_capture_vs_shared_burden,
    'Does the ministry capture the arrangement''s gains, or do bishops share the compliance burden symmetrically enough that no seat captures?',
    'Examine whether resemblance rulings systematically expand discretionary space versus constrain it, and compare ministry households'' compliance costs to member households''.',
    'If captured, the named-seat gain_flow stands and snare-flavor strengthens; if burden is broadly shared, extraction is more diffuse and the tangled_rope reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ministerial_capture_vs_shared_burden, empirical, 'Whether interpretive authority accrues as capture or is offset by shared burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(gela_tr_t0, observed).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(gela_tr_t10, observed).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(gela_tr_t20, observed).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(gela_tr_t30, observed).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement_basis(gela_tr_t40, observed).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__artifact_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement_basis(gela_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(gela_be_t0, observed).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(gela_be_t10, observed).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(gela_be_t20, observed).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(gela_be_t30, observed).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(gela_be_t40, observed).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__artifact_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(gela_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(gela_su_t0, observed).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(gela_su_t10, observed).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(gela_su_t20, observed).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(gela_su_t30, observed).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(gela_su_t40, observed).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__artifact_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement_basis(gela_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Amish separation' covers three structurally distinct claims with different epsilon values, victim sets, and failure modes. This story is the artifact_reading (resemblance criterion; highest epsilon; forbids functionally harmless technology). The principle_reading (entanglement criterion) and consequence_reading (effects criterion) are separate files. The artifact reading is upstream in enforcement terms - its rulings are cited as precedent in district disputes - while the siblings operate as live alternatives that members invoke against it. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
