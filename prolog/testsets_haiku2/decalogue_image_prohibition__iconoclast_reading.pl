% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Prohibition: All Religious Imagery as Idolatry
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The iconoclast reading of the Decalogue's prohibition against graven
 *   images interprets the commandment as categorically forbidding all
 *   material representation used in worship. Under this reading, any image
 *   serving a devotional function—whether a painted icon, an illuminated
 *   manuscript, a sculptural representation, or a visual aid to
 *   prayer—constitutes idolatry and violates the foundational law. This
 *   reading is ONE instantiation of a contested kernel (the Decalogue image
 *   prohibition). The sibling iconodule reading (separate constraint story)
 *   interprets the same commandment differently: it permits images as valid
 *   conduits to the divine through the prototype, distinguishing worship of
 *   an image (forbidden latria) from honor through an image to its prototype
 *   (permitted dulia). The moderate iconoclast reading (third sibling)
 *   permits two-dimensional images under regulation while forbidding
 *   three-dimensional statuary. This story instantiates ONLY the iconoclast
 *   reading as a coherent constraint with its own ε, beneficiary/victim
 *   structure, and type classification. The contest between readings is
 *   carried through omega variables and the cs_structure.reading_relations
 *   blocks, not through hedging within the constraint itself.
 *
 * KEY AGENTS:
 *   - centralizing_religious_authority: Theological center setting and enforcing the prohibition; derives authority and obedience by monopolizing access to the sacred — high directionality toward beneficiary end
 *   - icon_producers: Craftspeople, illuminators, sculptors whose trade is criminalized; exit would require abandoning their profession — trapped near target end
 *   - monastic_communities: Devotional institutions whose visual piety and scholarly traditions depend on images; identity-locked to image-based practice they are now condemned for — high extraction, identity-locked exit
 *   - devotional_practitioners: Lay believers whose prayer and meditation relied on visual imagery; access to the sacred is severed; identity-locked to devotional habits the prohibition condemns — powerless, identity-locked, high extraction
 *   - imperial_enforcement_apparatus: State machinery consolidating political power by enforcing doctrinal monopoly and eliminating competing institutional authority — high directionality toward beneficiary end
 *   - competing_iconodule_authority: Alternative theological centers and image-venerating communities excluded from authority by the enforcement apparatus — structurally barred from the conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.78).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.81).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Prohibition: All Religious Imagery as Idolatry").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '87425863-8d64-4139-96bd-2af8fb6804f0').
narrative_ontology:cs_kernel_codification('87425863-8d64-4139-96bd-2af8fb6804f0', fixed_text).
narrative_ontology:cs_authority_grounding('87425863-8d64-4139-96bd-2af8fb6804f0', lineage).
narrative_ontology:cs_interpretation_layer_present('87425863-8d64-4139-96bd-2af8fb6804f0').
narrative_ontology:cs_reading_relation('87425863-8d64-4139-96bd-2af8fb6804f0', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('87425863-8d64-4139-96bd-2af8fb6804f0', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('87425863-8d64-4139-96bd-2af8fb6804f0', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('87425863-8d64-4139-96bd-2af8fb6804f0', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('87425863-8d64-4139-96bd-2af8fb6804f0', foundational, all_imagery_constitutes_idolatry_risk).
narrative_ontology:cs_axiom_status(all_imagery_constitutes_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('87425863-8d64-4139-96bd-2af8fb6804f0', all_imagery_constitutes_idolatry_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('87425863-8d64-4139-96bd-2af8fb6804f0', material_prohibition_spiritual_necessity).
narrative_ontology:cs_drift_state('87425863-8d64-4139-96bd-2af8fb6804f0', post_iconodule_revival_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('87425863-8d64-4139-96bd-2af8fb6804f0', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_religious_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_enforcement_apparatus).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, monastic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The theological and administrative center (imperial church council, doctrinal authority, centralized clergy hierarchy) that declares the prohibition, interprets it, and wields it to consolidate control over devotional practice and eliminate competing forms of access to the sacred. The reading monopolizes mediation of the holy by forbidding all material intermediaries except those the authority explicitly permits (texts, theologically approved teaching). Collects authority and obedience directly; extracts through the power to define what counts as idolatry.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_religious_authority, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Craftspeople, artists, monasteries, and manuscript illuminators whose labor produces religious imagery. Under the iconoclast reading, all such work becomes prohibited—their trade is criminalized, their products destroyed, their skills rendered valueless. Exit would require abandoning their profession entirely and accepting the authority's redefinition of their former work as idolatry. Suppression is both legal (prohibition, confiscation) and ideological (moral condemnation of their craft as spiritually dangerous).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, constrained, universal).

% Communities whose devotional practice, liturgical tradition, and scriptural transmission depend on illuminated texts, sacred images, and visual piety. The iconoclast reading demands the destruction of their accumulated visual culture and the abandonment of image-based meditation and intercessory prayer. Their institutional identity is bound to a tradition the reading now condemns; exit means ceasing to be what they are. They also benefit incidentally from some aspects of centralized authority (protection, institutional status) but bear the primary cost of image destruction.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, monastic_communities, beneficiary).

% Lay believers whose spiritual practice relies on images—icons for prayer, visual reminders of holy figures, illuminated manuscripts for meditation. The iconoclast reading condemns their devotional habits as idolatry and demands they abandon visual piety. For many, the image IS their access to the sacred; the prohibition severs that access and offers no equivalent alternative. Suppression is enforced through confession, social shame, and threat of damnation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, identity_locked, universal).

% The state machinery (imperial courts, military, local officials) that enforces the prohibition through confiscation, destruction of images, punishment of producers and practitioners, and control of trade. The enforcement apparatus consolidates political power by centralizing the definition of religious correctness and eliminating the competing institutional authority of image-venerating monasteries and local cult practices. Extracts obedience and demonstrates state control over doctrine.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, universal).

% Alternative theological centers, competing bishops, and monastic networks that would defend image veneration as doctrinally legitimate and salvifically necessary. They are structurally barred from the authoritative conversation by the iconoclast reading's control of enforcement machinery. Their exclusion is what the suppression apparatus exists to maintain; were they admitted to define doctrine, the entire prohibition would collapse.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, competing_iconodule_authority, excluded,
    institutional, civilizational, trapped, universal).

% Historians, theologians, and scholars examining whether the prohibition emerges from genuine scriptural exegesis or from the structural need of centralizing authority to monopolize access to the sacred. They assess whether the reading's claimed justification (preventing idolatry through material prohibition) is matched by its actual operation (consolidating power through image destruction and producer elimination).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, theological_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, centralizing_religious_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining unified doctrinal boundaries and preventing fragmentation of practice through private or local devotional imagery: if every community produces its own visual piety, theological interpretation diffuses and authority becomes decentralized. A blanket prohibition on all religious imagery centralizes the definition of correct belief and eliminates parallel paths to the sacred.
% TRANSFER_FUNCTION: Transfers authority over the sacred—from distributed local and monastic communities to the centralized religious authority—by making all material mediation of the holy illegal and requiring that access to the sacred flow only through authorized (typically textual, hierarchical, non-visual) channels. Transfers wealth from icon producers and scriptoria to the enforcement apparatus. Transfers power of definition from competing authorities to the monopoly authority.
% ABSENT_VOICES: Iconodule theologians, image-venerating monasteries, lay devotional practitioners dependent on visual piety, and icon-producing communities. These parties would argue the prohibition is a power grab rather than theological necessity, that the Incarnation sanctifies matter, and that visual devotion enables rather than obstructs genuine piety. They are excluded from authority by the same enforcement machinery that carries the prohibition.
% DISAPPEARANCE_RATIONALE: If the prohibition and its enforcement disappeared overnight, icon production would resume within weeks, devotional communities would rebuild their visual culture, competing theological authorities would re-establish image veneration, monastic scriptoria would recommence illumination, and the centralizing authority's monopoly over doctrinal definition would fragment. The constraint is what holds the consolidation together; without it, power redistributes immediately.
% FOUNDING_PROBLEM: The proliferation of image veneration in localized cults, competing monastic traditions, and lay devotion was understood by centralizing authorities as a threat to unified doctrine and hierarchical control. Images allowed direct access to the sacred without mediation by the official priesthood, and image-venerating communities wielded independent authority. The prohibition was built to eliminate that structural independence and consolidate all access to the sacred through the central authority.
% FOUNDING_PROBLEM_CORROBORATION: The centralizing authority attests the founding problem is preventing idolatry and protecting doctrine. Iconodule theologians and excluded communities attest the problem is actually institutional independence: the founding problem was not that images are spiritually dangerous, but that images enabled communities to bypass the authority's gatekeeping. Historical and theological analysis from outside the benefiting parties supports the power-consolidation reading.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The constraint is classified as tangled_rope (not pure snare) because it genuinely solves a coordination problem—preventing fragmentation of doctrine and maintaining unified theological boundaries—while simultaneously extracting authority and power asymmetrically. Extractiveness is high (0.78) because the constraint's operation is substantially decoupled from the severity of the idolatry problem it claims to solve: the prohibition extends to all imagery without distinction of context or intent, banning illuminated scripture and private devotional practice equally. Suppression is higher still (0.81) because persistence depends on actively destroying images, eliminating producers, and barring competing authorities—not on participant preference for centralization but on coerced compliance enforced through legal punishment, social shame, and threat of damnation. Theater ratio rises from 0.28 to 0.42 over the interval: early in enforcement, genuine image destruction and producer elimination dominates; as resistance accumulates and alternatives persist, more enforcement activity becomes ritualized displays of piety (public image-burning ceremonies, theological argumentation against images, confessions of repentance) to maintain the prohibition's legitimacy without achieving complete suppression. The constraint plateaus in its final state: extraction and suppression stabilize at high levels, but the ratio of performative to functional activity (theater_ratio near 0.42) indicates the enforcement machinery must work harder to maintain compliance as the foundational problem (preventing doctrinal fragmentation) has been substantially achieved and the constraint's operation shifts toward pure rent-extraction of authority. The shared temporal grid means every metric is authored at every examined time point; no measurement series omits endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (centralizing_religious_authority and imperial_enforcement_apparatus) and the payer seats (icon_producers, monastic_communities, devotional_practitioners) should compute dramatically differently from identical structural data. From the authority seat, the arrangement is genuine coordination—eliminating the spiritual danger of idolatry and the institutional danger of fragmentation. From the payer seats, the same structure operates as enforced power consolidation: the authority uses the idolatry frame to justify destruction of competing sources of sacred access and independent communities. The engine computes this divergence from the power atoms, exit options, and directionality declarations; the commentary explains WHY the same constraint looks like coordination from one seat and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: centralizing_religious_authority and imperial_enforcement_apparatus both sit near the beneficiary end of directionality (d near 0.0 to 0.2). The authority gains monopoly power over doctrinal definition and the definition of correct piety. The apparatus gains political consolidation by enforcing the monopoly and eliminating competing institutional authorities. Both have high exit optionality (arbitrage) relative to the constraint—they could choose different enforcement strategies, but this one amplifies their power. Victims: icon_producers, monastic_communities, and devotional_practitioners sit near the target end (d near 0.8 to 1.0). Icon producers face criminalization of their trade with constrained exit—they must abandon their profession or hide their work. Monastic communities are identity-locked (their entire tradition and practice is now condemned) with no genuine exit path without ceasing to be what they are. Devotional practitioners are powerless, identity-locked to devotional habits the prohibition condemns. All three groups bear direct costs (confiscation, destruction, punishment) with minimal compensation. Competing_iconodule_authority is excluded by the enforcement structure itself—their exclusion is what the suppression apparatus exists to maintain—so they don't appear in the standard directionality derivation. The declared beneficiaries and victims feed the engine's directionality computation; overrides are not needed because the structural data accurately reflects the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope (not merely snare) because its mandate—preventing doctrinal fragmentation through image prohibition—is LIVE and ACHIEVED. The founding problem (preventing independent communities from wielding authority through image-based piety) remains valid; the prohibition successfully achieves that coordination. However, the constraint now persists partially as pure extraction BEYOND what the mandate requires: suppressing all imagery (including theologically neutral illuminated texts and private meditation images) is enforcement scope that exceeds the founding problem. This is the tangled_rope signature: genuine coordination function + asymmetric extraction through the same structure. If the founding problem were DEAD (if doctrinal fragmentation were no longer a live concern), the classification would shift toward snare; if the constraint had no beneficiary beyond the archive of compliance, it would be piton. The present state is tangled_rope: coordination is real (maintains unified doctrine), extraction is asymmetric (consolidates power), enforcement is active (required to hold both).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Decalogue''s prohibition against graven images a categorical material ban (iconoclast reading) or a permission for images as valid conduits to the divine (iconodule reading)?',
    'Exegetical analysis from outside the benefiting parties (theological historians, comparative religious studies); empirical observation of whether image-using communities demonstrate materially different outcomes in doctrinal coherence or spiritual corruption; natural experiments from theological traditions that adopted different readings.',
    'If the iconoclast reading is correct, the prohibition is a genuine natural law of spiritual safety enforced by an institutional structure. If the iconodule reading is correct, the prohibition is a false summit—a power grab dressed in theological language. The engine''s differentiation of the two readings as separate constraints with different ε values and beneficiary structures models this structural uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The interpretive foundation of the constraint: is the prohibition a material commandment or a spiritually permissive framework?').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of image production and devotional imagery sustained by structural barriers (legal prohibition, confiscation, enforcement) or by internalized conviction that images are idolatrous?',
    'Post-enforcement suppression trajectory: if image production and devotion resume rapidly when legal enforcement ceases (as it did historically during iconodule resurgence periods), the suppression is primarily structural; if practitioners remain image-averse after enforcement ends, suppression is partially internalized.',
    'If suppression is structural, the constraint''s effective extraction is high—the authority must work continuously to maintain it. If internalized, the authority captures a more stable monopoly with lower ongoing enforcement cost. The difference informs whether the constraint is sustainable as a tangled_rope or whether it will degrade to piton (performative maintenance) when enforcement capacity weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression persists as internalized belief or requires continuous structural enforcement.').

omega_variable(
    coordination_function_specificity,
    'Is the coordination function served by image prohibition specifically dependent on categorical material ban, or could the same doctrinal coherence be achieved through content regulation (approved vs. unapproved images)?',
    'Comparative analysis of theological systems using image regulation instead of prohibition: do regulated-image systems show equivalent doctrinal fragmentation or different failure modes?',
    'If categorical prohibition is structurally necessary for doctrinal unity, the enforcement scope is justified by the coordination function. If content regulation achieves the same outcome, the categorical scope is extractive overhead—the constraint extends beyond what the mandate requires and becomes more purely extractive (snare-flavored). This would shift the measured proportion of extraction that is coordination cost versus monopoly rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_specificity, empirical, 'Whether the prohibition''s scope exceeds what the coordination mandate requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The Decalogue image prohibition is a contested kernel with three distinct readings. This story instantiates the iconoclast reading (categorical ban on all imagery). The iconodule reading (images as valid conduits through prototypes) and moderate iconoclast reading (three-dimensional ban, two-dimensional regulation) are separate constraints, each with its own ε, beneficiary/victim structure, and classification. All three are linked via network.affects_constraints because they compete to interpret the same scriptural text and each reading's authority affects the feasibility of the others. ε-invariance: the three readings have substantially different ε values—the iconoclast reading treats all imagery as extraction-adjacent (high ε), the iconodule reading treats image mediation as coordination cost (lower ε), the moderate reading treats regulation as a balanced gate (intermediate ε). They are not the same constraint viewed from different angles; they are different constraints instantiating different interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
