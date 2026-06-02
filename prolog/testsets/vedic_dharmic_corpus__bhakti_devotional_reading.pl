% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine Authority (Vedic Dharmic Corpus)
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   The bhakti devotional reading of the vedic dharmic corpus instantiates an
 *   alternative legitimacy claim: sincere devotional practice (bhakti) rather
 *   than ritual-birth qualification (caste) determines spiritual authority
 *   and divine access. This reading emerges in tension with the hereditary
 *   brahminical monopoly reading, which grounds legitimate authority in vedic
 *   ritual expertise inherited through brahmin birth. The bhakti reading does
 *   not deny the vedic canon — it reinterprets it through devotional
 *   hermeneutics, emphasizing passages about divine mercy, emotional
 *   connection, and spiritual achievement over ritual prescriptions and
 *   status hierarchy. The constraint operates as a **transformation of
 *   authority structure**: the brahminical monopoly is not formally
 *   overthrown but is displaced from primary legitimacy-granting mechanism to
 *   secondary verification framework. Bhakti practitioners cite vedic
 *   authority to support devotionalism (still appealing to the canon), but
 *   the canon's coercive force is diminished — compliance with brahminical
 *   ritual hierarchy is no longer required for spiritual legitimacy. This
 *   creates a structural tension: the constraint simultaneously maintains the
 *   vedic canon's authority (by citing it) and subverts that authority (by
 *   allowing alternative pathways). The measurement trajectory shows
 *   theater_ratio rising from 0.20 to 0.48 as brahminical institutions
 *   increasingly adopt and systematize bhakti interpretations, adding
 *   interpretive complexity and performative legitimacy-construction to what
 *   began as a radical egalitarian movement. Extractiveness rises modestly
 *   (0.28 → 0.38) as the constraint stabilizes into an institutional
 *   accommodation rather than remaining a liberatory transgression.
 *
 * KEY AGENTS:
 *   - Low-Caste and Outcast Practitioners: Primary beneficiaries (powerless/identity_locked) — gain access to divine authority and spiritual status bypassing caste barriers; experience liberation as identity-lock substitution
 *   - Bhakti Movement Communities (Sampradayas, Regional Networks): Organized actors (organized/constrained) — coordinate devotional practice and build alternative authority structures; remain subordinate to vedic canon; experience mixed coordination and extraction
 *   - Vedic Brahminical Authority: Primary institutional beneficiary initially threatened, then adaptive (institutional/arbitrage) — retain ultimate textual authority through adoption of bhakti interpretations; extract control over alternative pathways; prevent complete authority loss
 *   - Vedic Canon (Formalized Text and Commentary Tradition): Interpretive authority structure (institutional/arbitrage) — partially degraded but maintained through hermeneutic reinterpretation; theater increases as multiple readings coexist
 *   - Reformist Movements and Political Coalitions: Secondary organized actors (organized/constrained) — use bhakti egalitarianism as foundation for broader caste critique; constrained by brahminical institutional power
 *   - Analytical Observer (Cross-Religious Perspective): Sees coordination mechanism for managing legitimacy tension (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.38).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.42).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine Authority (Vedic Dharmic Corpus)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__bhakti_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '9446380a-e6a3-4ad6-819d-1cd078e8ac32').
narrative_ontology:cs_kernel_codification('9446380a-e6a3-4ad6-819d-1cd078e8ac32', fixed_text).
narrative_ontology:cs_authority_grounding('9446380a-e6a3-4ad6-819d-1cd078e8ac32', lineage).
narrative_ontology:cs_interpretation_layer_present('9446380a-e6a3-4ad6-819d-1cd078e8ac32').
narrative_ontology:cs_reading_relation('9446380a-e6a3-4ad6-819d-1cd078e8ac32', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('9446380a-e6a3-4ad6-819d-1cd078e8ac32', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('9446380a-e6a3-4ad6-819d-1cd078e8ac32', foundational, sincere_devotion_grants_spiritual_authority).
narrative_ontology:cs_axiom_status(sincere_devotion_grants_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('9446380a-e6a3-4ad6-819d-1cd078e8ac32', sincere_devotion_grants_spiritual_authority, deontological).
narrative_ontology:cs_axiom('9446380a-e6a3-4ad6-819d-1cd078e8ac32', foundational, vedic_canon_interpretively_plural).
narrative_ontology:cs_axiom_status(vedic_canon_interpretively_plural, holdable).
narrative_ontology:cs_axiom_grounding('9446380a-e6a3-4ad6-819d-1cd078e8ac32', vedic_canon_interpretively_plural, conventional).
narrative_ontology:cs_reference_frame('9446380a-e6a3-4ad6-819d-1cd078e8ac32', devotional_hermeneutic_legitimacy).
narrative_ontology:cs_drift_state('9446380a-e6a3-4ad6-819d-1cd078e8ac32', contemporary_brahminical_adaptation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9446380a-e6a3-4ad6-819d-1cd078e8ac32', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_practitioners_low_caste).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, vernacular_religious_tradition_keepers).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_priestly_monopoly).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, caste_hierarchical_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BHAKTI DEVOTEE (ROPE) — For a low-caste or outcast practitioner, bhakti devotionalism offers genuine coordination benefit: sincere devotion (emotional/spiritual labor) becomes a recognized pathway to divine access and social status within devotional communities, bypassing caste barriers. The constraint coordinates spiritual achievement with devotional practice rather than birth. However, the devotee remains identity-locked — they cannot exit their newfound devotional identity without severing the very relationship that bhakti constructs as liberatory. The devotee experiences this as pure coordination (rope) because the benefit is real and the coercion is minimal; exit means renouncing the spiritual identity itself.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: BHAKTI MOVEMENT ORGANIZED (TANGLED ROPE) — Organized bhakti communities (lineages, sampradayas, regional devotional networks) experience both genuine coordination and asymmetric extraction. They coordinate spiritual practice, ritual, and teacher-student transmission (true coordination benefit). But they also experience extraction: the constraint subordinates them to the brahminical textual canon (vedic authority still controls legitimacy standards), and their 'alternative' pathway is accepted only insofar as it does not challenge the vedic hierarchy directly. Movement groups are constrained — they can build communities but not formally deny vedic superiority without excommunication or political conflict. This is tangled rope because coordination is genuine (shared devotional practice) AND extraction is structural (subordinate legitimacy status).
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VEDIC PRIESTLY AUTHORITY (SNARE) — From the brahminical institutional perspective, bhakti devotionalism appears as a threat to the monopoly on legitimate religious authority. The constraint forces the priestly class to recognize alternative pathways to divine access (minimal coordination benefit — prevents outright rebellion). But the priestly class experiences this as extraction of authority: they must now defend their monopoly rather than assume it, suppress alternative interpretations of vedic texts (high suppression, high coercion), and tolerate rivals. They have arbitrage — they can always reassert vedic priority through textual interpretation — but the structural necessity to do so indicates extraction. From the beneficiary's perspective (high chi because they control textual interpretation), this is snare-adjacent but classified as snare because it models how the priestly class experiences constraint-driven loss of monopoly privilege.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HOUSEHOLDER DEVOTEE (TANGLED ROPE) — A householder (merchant, artisan, low-rank priest) who adopts bhakti devotionalism experiences tangled coordination and extraction. They benefit from the constraint's expansion of authority (can now claim spiritual status through devotion rather than birth alone) AND are constrained by it (must perform devotion continuously, integrate into devotional networks with their own hierarchies, navigate social disapproval from orthodox communities). Extraction is visible: they invest emotional labor and social risk; the benefit accrues as status within devotional circles but not universally. They are constrained — social and economic integration prevents exit — but the constraint is explicitly dual: genuine coordination of spiritual practice alongside real constraint.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: VEDIC CANON (PITON) — The vedic canon itself (formalized texts, commentary traditions, ritual prescriptions) is now partially degraded as an authority structure for determining religious legitimacy. Bhakti reading strategies (allegorical reinterpretation, emphasis on emotional truth over literal ritual compliance) have undermined the canon's monopoly on meaning-making without formally denying its authority. The canon persists through institutional inertia (still cited, still revered, still used to legitimize bhakti readings themselves). Theater ratio is high (0.65+): elaborate textual justifications of devotionalism appeal to vedic authority even as they subvert its literal prescriptions. The canon has become partially performative — maintained as legitimacy theater rather than as a functional constraint on practice.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a cross-religious perspective, bhakti devotionalism represents a genuine coordination mechanism: it solves the legitimacy crisis within a hierarchical religious system by creating an alternative pathway that does not require complete institutional overthrow. Compared to religious systems that must choose between monopoly or revolution, bhakti enables coexistence of hierarchies with alternatives. The analytical view sees this as pure coordination (rope) — the constraint serves its stated function (enabling devotional access) with minimal suppression or extraction once stabilized. However, this perspective risks obscuring the residual extraction experienced by those in lower-status devotional lineages relative to brahminical authority.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__bhakti_devotional_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, TR),
    TR >= 0.70.

:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The bhakti reading does provide genuine liberation for low-caste practitioners — access to divine authority, spiritual status, and community belonging bypass caste barriers. But the extraction is not zero because: (1) bhakti devotees remain subordinate to the vedic canon and brahminical institutions in the broader legitimacy hierarchy; (2) organized bhakti lineages develop internal hierarchies that replicate caste-like structures (guru status, seniority-based authority); (3) brahminical institutions progressively adopt and systematize bhakti, converting it from transgressive movement to institutional accommodation and subordinating it to vedic orthodoxy. The moderate value reflects that genuine liberation coexists with residual structural subordination. Suppression (0.42): Moderate. The constraint is actively enforced by brahminical authorities through textual reinterpretation, institutional gatekeeping (controlling which bhakti lineages are legitimate), and doctrinal claims that bhakti is compatible with vedic authority (theological suppression of full egalitarianism). But suppression is not extreme — devotees have real community autonomy and the constraint permits alternative pathways (not maximal coercion). Theater ratio (0.35 at baseline, rising to 0.48): Moderate and rising. Bhakti devotionalism begins with relatively low theater — genuine emotional practice and direct divine connection. But as the constraint stabilizes and brahminical institutions adopt it, theater increases: elaborate textual justifications, systematic guru hierarchies, ritual performances of devotion, and rhetorical appeals to vedic authority become necessary to maintain institutional legitimacy alongside original spontaneous devotion. The trajectory shows theater accumulation as the constraint moves from transgression to accommodation.
 *
 * PERSPECTIVAL GAP:
 *   The bhakti reading produces maximum perspectival divergence. A low-caste devotee sees rope (genuine liberation, minimal coercion, pure coordination of spiritual practice). A brahminical institutional observer sees snare (authority extraction, necessity to suppress alternatives, constrained arbitrage). An organized movement sees tangled rope (real coordination and real extraction simultaneously). The vedic canon itself appears as piton (degraded and partially performed rather than functionally enforced). A householder in mixed social position sees tangled rope (genuine spiritual benefit AND material constraint from social integration). The analytical observer sees pure rope (a coordination mechanism). The gap is maximal because the same structural feature — bhakti as alternative authority — produces opposite empirical predictions depending on the agent's structural position and stakes in the outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position: low-caste devotee (powerless/identity_locked) experiences low d (structurally beneficiary despite powerlessness, because they gain access to authority). Brahminical authority (institutional/arbitrage) experiences high d (structurally threatened, must suppress alternatives to maintain monopoly). Organized movement (organized/constrained) experiences moderate d (mixed beneficiary-victim position — genuine coordination benefit alongside institutional subordination). The piton perspective on the vedic canon itself is neutral-to-positive d (the canon benefits from continued legitimacy claims, even if now through multiple readings). The analytical perspective experiences near-zero d (observational position without structural stake). The directionality derivation reveals that the constraint's effect is to redistribute structural advantage: low-caste agents gain d-downward shift (less target), brahminical monopoly experiences d-upward shift (more target). No explicit overrides needed — the structural data (beneficiary/victim declarations + exit options) produces the correct d-values through automatic derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy between liberation and subordination narratives: bhakti is genuinely liberatory (rope-level coordination benefit) AND structurally subordinate (tangled rope with residual extraction). Both perspectives are accurate. The constraint manages this through dual legitimacy: devotional achievement is recognized as valid, but within the broader vedic hierarchy. The mandatrophy is dissolved by recognizing that 'valid alternative pathway' and 'subordinate to ultimate authority' are not contradictory — they coexist as the constraint stabilizes. The theater accumulation (measured as rising theater_ratio) indicates that the eventual equilibrium may drift toward piton if devotionalism becomes primarily performative legitimacy-maintenance. The brahminical perspective (snare) is the perspective of a class that has lost monopoly privilege; it accurately describes extraction OF authority but misses the genuine coordination of alternative spiritual practice that is real to devotees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_genuine_liberation,
    'Is bhakti devotional identity-lock a form of genuine spiritual liberation, or does it substitute caste-based identity lock with devotional-community identity lock?',
    'Post-exit analysis: when devotees leave devotional communities or abandon bhakti practice, do they report identity dissolution similar to caste-exit, or is departure experienced as different? Longitudinal study of life trajectories after bhakti exit vs caste-boundary crossing.',
    'If substitutive: bhakti is piton-adjacent (new theater replaces old), and victim set expands. If genuinely liberatory: the identity_locked exit option properly models the constraint, and victim set shrinks to caste monopoly alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_liberation, empirical, 'Whether bhakti liberation transcends or substitutes caste identity lock').

omega_variable(
    textual_hermeneutics_under_determination,
    'Does bhakti devotionalism represent a coherent alternative reading of vedic texts, or a fundamental reinterpretation that forecloses the hereditary monopoly reading?',
    'Formal analysis of vedic passages cited in bhakti vs hereditary monopoly readings: can both readings coexist within a single interpretive framework, or does adopting bhakti''s hermeneutic principles logically require rejecting hereditary monopoly principles?',
    'If coexistent: reading_relations should be coexists_with (current). If mutually foreclosing: should be forecloses (structural contradiction in core premises).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_hermeneutics_under_determination, conceptual, 'Whether bhakti and hereditary monopoly readings are compatible within vedic hermeneutics').

omega_variable(
    brahminical_adaptation_or_enclosure,
    'As brahminical institutions adopt bhakti interpretations (e.g., Advaita Vedanta, Shaivism), are they genuinely ceding authority, or enclosing and subordinating bhakti to vedic orthodoxy?',
    'Historical analysis of brahminical commentary on bhakti texts over time; institutional power distribution in sampradayas before and after brahminical adoption; whether bhakti-derived institutions maintain independent legitimacy authority or defer to vedic canon.',
    'If genuine ceding: extraction metric (0.38) is accurate and constraint is stabilizing rope/tangled rope. If enclosure: extraction rises and constraint becomes disguised snare with theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_adaptation_or_enclosure, empirical, 'Whether brahminical adoption of bhakti represents ceding or enclosure of authority').

omega_variable(
    devotional_lineage_hierarchy_reproduction,
    'Do bhakti devotional lineages reproduce caste-like hierarchies internally (guru status, ritual authority, community position based on birth/early initiation)?',
    'Ethnographic study of sampradaya internal organization: are leadership positions hereditary within devotional lineages? Do outcast/low-caste devotees rise to guru or institutional authority status? What barriers exist?',
    'If reproduction confirmed: victims set may expand to include internal devotional hierarchy; theater_ratio may rise (hierarchy maintained performatively via devotional language); constraint may degrade toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_lineage_hierarchy_reproduction, empirical, 'Whether bhakti lineages reproduce caste-like internal hierarchies').

omega_variable(
    kernel_reading_committer_frame,
    'Is this bhakti devotional reading a genuine live alternative to the vedic monopoly reading, or an aspirational narrative that brahminical authorities permit only rhetorically?',
    'Institutional and discursive analysis: which reading has primary authority in practice? Whose interpretation settles disputes? Who controls institutional resources and legitimacy certification?',
    'If live alternative: coexists_with or influences relations are correct. If rhetorical concession: the constraint is more snare-adjacent than rope, and axiom_status should be overridden rather than holdable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether bhakti reading is genuinely live or rhetorically conceded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhakti_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bhakti_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(bhakti_tr_t200, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(bhakti_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bhakti_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.36).
narrative_ontology:measurement(bhakti_be_t200, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, brahminical_institutional_gatekeeping).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, sampradaya_authority_legitimacy).

% DUAL FORMULATION NOTE:
% The bhakti devotional reading is one of three constraint stories in the vedic_dharmic_corpus kernel family. The inherited epsilon values differ structurally: hereditary_monopoly_reading (ε ≈ 0.15, rope/mountain from brahminical perspective) represents the unchanged monopoly baseline; bhakti_devotional_reading (ε ≈ 0.38, tangled rope) represents the liberatory-but-subordinate alternative; reformist_egalitarian_reading (ε ≈ 0.68, snare/tangled rope) represents radical caste rejection. Each reading's epsilon reflects its own structural position and extraction profile. The three stories are linked: hereditary monopoly is upstream (it is what bhakti challenges); bhakti influences reformist egalitarianism (which extends the logic further); brahminical gatekeeping affects all three (institutional enforcement); sampradaya authority is affected by all three (legitimacy source shifts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
