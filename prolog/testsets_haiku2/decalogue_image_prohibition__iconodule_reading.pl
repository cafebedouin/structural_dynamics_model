% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Image Veneration through Dulia/Latria Distinction (Iconodule Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading of the Decalogue's prohibition on graven images
 *   constructs a theological framework that permits veneration of religious
 *   imagery through a latria/dulia distinction: worship (latria) belongs to
 *   God alone, but honor (dulia) toward images of Christ, Mary, and saints is
 *   permissible because it directs honor through the image to its prototype.
 *   The Incarnation doctrine—that God became matter in Jesus—sanctifies
 *   material mediation as theologically sound. Under this reading, icons are
 *   not idolatrous but are aids to devotion and expressions of orthodox
 *   theology. The claim/metric gap is intentional: the reading is CLAIMED as
 *   rope (genuine coordination between laity and divine through sanctioned
 *   visual culture) while the metrics show substantial suppression (0.72) and
 *   rising theater (0.41 at interval end), indicating that a growing share of
 *   enforcement energy defends the theological framework itself against
 *   iconoclast challengers rather than maintaining the coordination function.
 *   This reading has been contested for over a millennium; the coercion grid
 *   shows class-level resistance to the constraint rising while
 *   organizational-level suppression hardens.
 *
 * KEY AGENTS:
 *   - Orthodox ecclesiastical authority: controls theological interpretation and enforces the latria/dulia distinction; collects authority over religious practice
 *   - Icon painters: licensed artisans; gain livelihood and status from ecclesiastical patronage under the reading
 *   - Laity through visual piety: powerless, identity-locked; gain access to devotional practice mediated by images
 *   - Iconoclast movements: powerful but persistently suppressed; argue the prohibition forbids all religious imagery
 *   - Persecuted icon-venerators: powerless, trapped; lose their practice when iconoclast readings dominate
 *   - Destroyed artworks: non-agents; casualties of the constraint's enforcement across competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.38).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.72).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Image Veneration through Dulia/Latria Distinction (Iconodule Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '4c02816e-302d-4a44-a75a-c2a8bb1b5cf0').
narrative_ontology:cs_kernel_codification('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', fixed_text).
narrative_ontology:cs_authority_grounding('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', lineage).
narrative_ontology:cs_interpretation_layer_present('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0').
narrative_ontology:cs_reading_relation('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', foundational, dulia_latria_distinction_coherent).
narrative_ontology:cs_axiom_status(dulia_latria_distinction_coherent, holdable).
narrative_ontology:cs_axiom_grounding('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', dulia_latria_distinction_coherent, deontological).
narrative_ontology:cs_reference_frame('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', incarnational_material_mediation).
narrative_ontology:cs_drift_state('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', post_reformation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c02816e-302d-4a44-a75a-c2a8bb1b5cf0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity_through_visual_piety).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, iconoclast_movements).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, persecuted_icon_venerators).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, destroyed_artworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which images are theologically sound and permits their veneration under the dulia framework. Enforces the latria/dulia distinction by reviewing iconography, suppressing heterodox depictions, and prosecuting those who treat images as objects of worship rather than aids to devotion. Maintains both the theological coherence of the reading and its institutional authority to interpret scripture.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesiastical_authority, agenda_setter,
    institutional, civilizational, trapped, continental).

% Gain commissions and social standing within the Church as licensed producers of approved religious imagery. Must submit designs for ecclesiastical review and adhere to iconographic canons; their work is sanctioned only when it conforms to orthodox theology. A thriving market for icons exists because the reading permits and even celebrates the visual arts.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters, beneficiary,
    moderate, biographical, constrained, continental).

% Access the divine through veneration of icons—a form of prayer mediated by images. The constraint permits this practice and frames it as orthodox devotion, not idolatry. For the largely illiterate laity, icons serve as visual scripture; the reading legitimizes this as spiritually valid and theologically sound.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity_through_visual_piety, beneficiary,
    powerless, biographical, identity_locked, continental).

% Face suppression and theological denunciation. Where the iconodule reading dominates, iconoclast preachers are branded as heretics, their texts burned, and their movements persecuted. The dulia/latria framework is deployed to delegitimize their core claim (that the prohibition forbids all religious imagery). The cost they bear is loss of institutional authority, exile, and death in severe cases.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_movements, payer,
    powerful, generational, trapped, continental).

% In periods when iconoclast readings dominate (e.g. Byzantine Iconoclasm 8th–9th centuries), icon-venerators are hunted, their icons destroyed, their practice criminalized as idolatry. Even when the iconodule reading regains authority, memory of persecution lingers. Their suppression is imposed by the same scriptural reading applied from the opposite theological pole.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, persecuted_icon_venerators, payer,
    powerless, biographical, identity_locked, continental).

% Religious art destroyed during iconoclast periods (iconoclasm in Byzantium, later Reformation periods): mosaics smashed, panel icons burned, sculptures melted. Under the iconodule reading these works are sacred expressions of theological truth; under the iconoclast reading they are idolatrous violations. The constraint's enforcement—whichever reading dominates—determines whether artworks are preserved or destroyed.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, destroyed_artworks, payer,
    powerless, immediate, trapped, continental).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconodule_reading, destroyed_artworks).

% When iconoclast readings dominate, the laity lose access to visual devotion—the primary form of religious practice for the illiterate. Their exclusion from the conversation is structural: they cannot argue for the spiritual validity of icon-veneration in theological forums; their piety is redefined as idolatry by authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity_under_iconoclasm, excluded,
    powerless, biographical, trapped, continental).

% Analyze the scriptural foundation of the Incarnation doctrine and the permissibility of material mediation. They produce arguments that privilege the iconodule reading, but also acknowledge alternative exegetical traditions. Their analysis feeds into conciliar decisions and theological authority structures.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_exegetes, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesiastical_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the scriptural prohibition on graven images with the pastoral need for visual devotion by introducing a theological framework (latria/dulia) that permits material mediation under specified conditions (proper intention, orthodox depiction, honor directed through the image to its prototype, not to the image itself). Solves the coordination problem of how a largely illiterate laity can access religious practice without violating monotheistic law.
% TRANSFER_FUNCTION: Transfers theological authority to the ecclesiastical hierarchy (which interprets the latria/dulia distinction); transfers artistic livelihood to icon-painters licensed by the Church; transfers access to visual devotion to the laity on condition of accepting the Church's theological framework; suppresses iconoclast theology and the communities that hold it.
% ABSENT_VOICES: Iconoclast theologians (whose alternative reading of the Decalogue is suppressed); laity in regions where iconoclast authority dominates (whose visual piety is criminalized); Islamic and other non-Incarnational theologians (whose rejection of the Incarnation doctrine challenges the reading's foundation); Reformed and Anabaptist Christian communities that affirm the Incarnation but reject material mediation. These voices are structurally excluded from authoritative theological conversation when the iconodule reading holds institutional power.
% DISAPPEARANCE_RATIONALE: If the dulia/latria framework and its institutional enforcement vanished, visual religious practice would either revert to iconoclast prohibition (removing images from worship) or would shift toward folk-style veneration outside ecclesiastical control. Icon-painting as a sanctioned profession would shrink. Theological authority would disperse among competing Christian traditions. The ecclesiastical monopoly on interpreting the Incarnation doctrine would erode.
% FOUNDING_PROBLEM: The early Church faced an apparent contradiction: the Decalogue forbids graven images (Exodus 20:4–5), yet the Incarnation doctrine claims that God became matter, visual, tangible. How can the laity legitimately access devotion through material images without violating the law against idolatry?
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities (councils, church fathers like John of Damascus, Eastern Orthodox tradition) attest the founding problem is live and permanent: the Incarnation requires material mediation as a valid spiritual path. Iconoclast theologians and Protestant reformers argue the problem is false: the prohibition is clear and the Incarnation does not require visual practice. Islamic theology, which affirms God's absolute transcendence and rejects the Incarnation, challenges whether the founding problem's premises are sound. Modern Christian historians from outside the iconodule tradition (Protestant, secular scholars) document the political and social dimensions of the dispute, showing the constraint was contested even among those who affirmed both the Decalogue and the Incarnation. Corroboration is mixed and comes from outside the beneficiary circle.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.22 at t0) because the constraint serves a genuine coordination function—laity gain access to devotion, icon-painters gain livelihood, ecclesiastical authority gains control over theological interpretation—but also extracts from iconoclasts by suppressing their alternative reading. The suppression metric (0.72) is elevated because the constraint's persistence depends on actively defending the theological framework against iconoclast challenge; the enforcement machinery includes text-burning, exile, and prosecution. Theater ratio rises from 0.15 to 0.41 over the interval, indicating increasing performative maintenance of the constraint—elaborate liturgical justifications and theological elaboration grow more prominent relative to the functional coordination role. The coercion grid shows class-level resistance (0.78 to 0.76) and individual-level resistance (0.85 to 0.80) remaining high, suggesting the reading never fully suppresses iconoclast or folk-practice alternatives. Suppression at organizational and structural levels hardens (0.62 to 0.73, 0.58 to 0.71), indicating the institutional Church strengthens enforcement machinery over time. The accessibility collapse metric is moderate-high (0.68): once the reading is embedded in authority structures, alternatives become difficult to access, but iconoclast theology and practice are never fully foreclosed—they re-emerge in the Reformation and in rival Christian traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical authority seat, the constraint is rope: it solves a coordination problem (how to reconcile scripture with lived practice) and enables a thriving visual culture under theological guidance. From the iconoclast seat, the same constraint is snare: it extracts authority to enforce a controversial theological reading and suppresses alternative interpretations of scripture. From the laity seat, the constraint is ambiguous—it permits their cherished visual devotion but under ecclesiastical control, which shifts power away from grass-roots piety toward institutional mediation. The engine computes these divergences from the structural data (beneficiary/victim declarations, exit options, power atoms); the authored claim (rope) does not resolve them. The coercion grid shows this perspectival gap spatially: individual-level resistance is high (laity practice folk-style veneration despite the theological framework) while organizational-level suppression is high (Church enforces the framework through hierarchy). The constraint works differently at different levels because the reading's cost structure is not symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox ecclesiastical authority has d near beneficiary end (controls interpretation, extracts authority, arbitrage-grade exit through institutional hierarchy). Icon-painters have d near beneficiary end (gain livelihood and status) but are slightly constrained (must conform to canons—d ≈ 0.35). Laity through visual piety have d near symmetric (d ≈ 0.52): they gain devotional access but must accept Church's theological framework and cannot practice without ecclesial approval. Iconoclast movements have d near target end (d ≈ 0.88): they are trapped (religious authority requires their suppression), have high resistance costs, and exit only via schism or forced conversion. Persecuted icon-venerators under iconoclasm have d at full target (d = 1.0): trapped by identity, suppressed by law, offering no coordination benefit to the dominant iconoclast authority. The destroyed artworks are non-agents (agent=false in stakeholder record) and are casualties of the constraint's operation across readings, not agents with directionality of their own.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure mandatrophy until the late interval (t>900), where theater ratio stabilizes around 0.41 and extractiveness flattens at 0.38—suggesting the constraint has reached an equilibrium where both function and performance are stable. The founding problem (reconciling Decalogue prohibition with Incarnation theology) remains LIVE across the interval, preventing classification as a zombie constraint. However, the contested founding-problem status and the rise of theater suggest a secondary mandatrophy risk: the constraint's function increasingly becomes performing the distinction (producing theological justifications) rather than enabling the coordination it claims (visual piety). The measurement series show theater rising fastest in the early-to-middle interval (t0–t600: 0.15→0.38) precisely when suppression is hardening (t0–t600: 0.58→0.76), indicating the constraint's primary function shifted from coordination to enforcement-and-justification. By the late interval (t600–t1200), theater stabilizes, suggesting the constraint has become embedded enough that continuous performative defense is routine rather than escalating. The coercion grid shows individual-level resistance remaining high (0.85→0.80) even as organizational-level suppression hardens, indicating the constraint never fully achieves functional dominance—laity continue folk practices that the framework technically forbids. This is not quite mandatrophy (the constraint still enables coordination for some seats) but is consistent with a Piton-ward trajectory where the theological edifice persists partly through inertia and performance rather than through the coordination benefit alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_enforceability,
    'Is the distinction between worship of an image (latria, forbidden) and honor toward an image''s prototype through the image (dulia, permitted) theoretically coherent and practically enforceable, or does it collapse under scrutiny into either permitting all image-veneration or forbidding all?',
    'Empirical study of lay practice: do laity consistently distinguish latria from dulia in their own piety, or do they conflate the two? Historical documentation of heresy trials and theological disputes shows where authorities enforced the distinction and where it broke down.',
    'If the distinction is enforceable and laity maintain it, the constraint is genuinely rope-type (coordination with low extraction). If the distinction collapses and laity treat all image-veneration as functionally equivalent, the reading is snare-type (extraction through theological cover story) or piton-type (a performance without functional substance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_enforceability, empirical, 'Whether the latria/dulia distinction is theologically coherent and socially maintained in practice.').

omega_variable(
    incarnation_doctrine_necessity,
    'Does the Incarnation doctrine REQUIRE material mediation in worship, or does it merely permit it? Is visual piety necessary to Christian theology or is it a contingent practice that the doctrine allows but does not necessitate?',
    'Exegetical study of Incarnation theology in Christian tradition; comparison with Christian communities (some Reformed, Anabaptist) that reject the iconodule framework but affirm the Incarnation. If robust Christian theology exists without material mediation, the iconodule reading''s claim that matter is sanctified as a ''valid conduit'' becomes contestable.',
    'If Incarnation theology allows but does not require visual piety, the constraint''s coordination function is weaker than claimed—it becomes more nearly a matter of ecclesiastical authority (who decides what the Incarnation implies) than a true coordination solution. The reading would be closer to snare than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incarnation_doctrine_necessity, conceptual, 'Whether the Incarnation doctrine necessitates or merely permits material mediation in worship.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of one kernel (the Decalogue image prohibition) or is it better understood as instantiating two separate kernels—the Decalogue, and the Incarnation doctrine—that are being woven together?',
    'Theological genealogy: trace how the latria/dulia framework emerged historically and what kernel texts it appeals to. If the reading primarily reinterprets the Decalogue using Incarnation theology as a hermeneutical key, it is one reading of one kernel. If the reading equally invokes both kernels as independent authorities, it may be a synthesis of two kernels rather than a reading of one.',
    'Single-kernel reading (this story''s assumption): the iconodule reading interprets the Decalogue through the lens of Incarnation theology. Two-kernel scenario: the constraint would belong to a different family (Incarnation-theology constraints) and the omegas would be reframed. Classification impact is minimal (both framings are rope-type) but the network structure of the constraint family would change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the constraint is a reading of the image-prohibition kernel or a synthesis of two kernels.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression primarily structural (coercive authority, threat of punishment, institutional enforcement) or internalized (laity have adopted the ecclesiastical theology and police their own practice, enforce it through social judgment)? How much of the suppression persists through psychological identification with the framework versus external coercion?',
    'Ethnographic and historical evidence: in periods when iconoclast authority dominates but iconodule practice persists (e.g., in peripheral regions or lower clergy), does the laity perform compliance while privately maintaining folk-style veneration (structural suppression only)? Or do they experience cognitive dissonance and guilt, indicating internalized suppression? Post-suppression trajectory: where icon-venerators were able to return to practice after iconoclast persecution, did suppression lift immediately or did residual identity-fusion prevent reversion?',
    'If suppression is structural, removing the ecclesiastical enforcement machinery would quickly restore alternative practices. If internalized, the identity-fusion between laity and the iconodule framework would persist even if external enforcement eroded—the constraint would show piton dynamics rather than snare. High internalization supports the rope claim (genuine coordination, not extraction); high structural suppression with low internalization supports the snare reading (extraction maintained by coercion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized mechanisms in the suppression of iconoclast alternatives.').

omega_variable(
    kernel_reading_contested,
    'This constraint is one of THREE readings of the decalogue_image_prohibition kernel. The sibling iconoclast reading claims the prohibition forbids all religious imagery. How are these readings related structurally? Does one reading logically foreclose the other within a single theological framework, or do they coexist as live options held by different parties?',
    'Theological analysis: Can a single Christian theologian coherently hold both readings (e.g., saying ''the prohibition forbids worship of images'' and separately ''the prohibition forbids all imagery except under dulia'')? Or do the readings rest on incompatible premises about what the Decalogue means, such that commitment to one logically rules out the other?',
    'If foreclosing: one reading logically eliminates the other; the constraint has stronger boundaries and the readings are genuinely alternatives. If coexisting: both readings are live options within Christianity; the constraint''s boundaries are more permeable and the readings are persistent differences without logical elimination. Influences relationship: one reading creates structural pressure on the other (e.g., ecclesiastical authority enforces one reading, which suppresses the other, which creates resistance that resurfaces) without logically foreclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'The structural relationship between the iconodule and iconoclast readings of the image-prohibition kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=1200
narrative_ontology:measurement(deca_grid_01, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(deca_grid_02, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(class), 1200, 0.66).
narrative_ontology:measurement(deca_grid_03, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(deca_grid_04, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(individual), 1200, 0.65).
narrative_ontology:measurement(deca_grid_05, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(deca_grid_06, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(organizational), 1200, 0.71).
narrative_ontology:measurement(deca_grid_07, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(deca_grid_08, decalogue_image_prohibition__iconodule_reading, accessibility_collapse(structural), 1200, 0.68).
narrative_ontology:measurement(deca_grid_09, decalogue_image_prohibition__iconodule_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(deca_grid_10, decalogue_image_prohibition__iconodule_reading, resistance(class), 1200, 0.76).
narrative_ontology:measurement(deca_grid_11, decalogue_image_prohibition__iconodule_reading, resistance(individual), 0, 0.85).
narrative_ontology:measurement(deca_grid_12, decalogue_image_prohibition__iconodule_reading, resistance(individual), 1200, 0.8).
narrative_ontology:measurement(deca_grid_13, decalogue_image_prohibition__iconodule_reading, resistance(organizational), 0, 0.81).
narrative_ontology:measurement(deca_grid_14, decalogue_image_prohibition__iconodule_reading, resistance(organizational), 1200, 0.82).
narrative_ontology:measurement(deca_grid_15, decalogue_image_prohibition__iconodule_reading, resistance(structural), 0, 0.71).
narrative_ontology:measurement(deca_grid_16, decalogue_image_prohibition__iconodule_reading, resistance(structural), 1200, 0.78).
narrative_ontology:measurement(deca_grid_17, decalogue_image_prohibition__iconodule_reading, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(deca_grid_18, decalogue_image_prohibition__iconodule_reading, stakes_inflation(class), 1200, 0.72).
narrative_ontology:measurement(deca_grid_19, decalogue_image_prohibition__iconodule_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(deca_grid_20, decalogue_image_prohibition__iconodule_reading, stakes_inflation(individual), 1200, 0.65).
narrative_ontology:measurement(deca_grid_21, decalogue_image_prohibition__iconodule_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(deca_grid_22, decalogue_image_prohibition__iconodule_reading, stakes_inflation(organizational), 1200, 0.58).
narrative_ontology:measurement(deca_grid_23, decalogue_image_prohibition__iconodule_reading, stakes_inflation(structural), 0, 0.51).
narrative_ontology:measurement(deca_grid_24, decalogue_image_prohibition__iconodule_reading, stakes_inflation(structural), 1200, 0.62).
narrative_ontology:measurement(deca_grid_25, decalogue_image_prohibition__iconodule_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(deca_grid_26, decalogue_image_prohibition__iconodule_reading, suppression(class), 1200, 0.72).
narrative_ontology:measurement(deca_grid_27, decalogue_image_prohibition__iconodule_reading, suppression(individual), 0, 0.54).
narrative_ontology:measurement(deca_grid_28, decalogue_image_prohibition__iconodule_reading, suppression(individual), 1200, 0.72).
narrative_ontology:measurement(deca_grid_29, decalogue_image_prohibition__iconodule_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(deca_grid_30, decalogue_image_prohibition__iconodule_reading, suppression(organizational), 1200, 0.73).
narrative_ontology:measurement(deca_grid_31, decalogue_image_prohibition__iconodule_reading, suppression(structural), 0, 0.58).
narrative_ontology:measurement(deca_grid_32, decalogue_image_prohibition__iconodule_reading, suppression(structural), 1200, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The 'decalogue_image_prohibition' kernel has three structurally distinct readings, each with different ε values and victim sets. The iconodule reading (this story) has ε≈0.38 and claims rope-type coordination; the iconoclast reading has higher ε (extraction through suppression of visual practice) and claims snare; the moderate reading occupies a middle position. All three readings reference the same kernel (the Decalogue's image prohibition and Christian theology of the Incarnation) but interpret it differently. They are linked as a constraint family via affects_constraints. No single reading is the 'right' one; the framework models them as three distinct constraints derived from one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
