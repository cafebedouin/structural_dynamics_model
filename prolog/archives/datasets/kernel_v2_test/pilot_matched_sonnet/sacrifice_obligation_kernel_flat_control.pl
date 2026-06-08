% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel_flat_control
 *   human_readable: Halakhic Obligation to Perform Temple Sacrifice
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The halakhic obligation to perform Temple sacrifice represents a
 *   constraint whose functional content ceased in 70 CE with the destruction
 *   of the Second Temple but whose formal legal status persists within the
 *   halakhic system. The interval spans from the Temple's destruction (year 0
 *   in this model) to the present (1,954 years later). The constraint
 *   exhibits a clear piton signature: theater_ratio has increased from 0.70
 *   to 0.85 as the obligation's maintenance has become increasingly
 *   performative (textual study, liturgical commemoration, eschatological
 *   hope) while its functional content (actual sacrifice) remains absent.
 *   Base extractiveness has decreased from 0.30 to 0.15 over the same period,
 *   reflecting the halakhic system's gradual adaptation: the Rabbinic
 *   substitution doctrine ('our lips replace the bulls') and the
 *   normalization of prayer as the primary mode of divine service have
 *   reduced the cognitive and social cost of non-performance. The constraint
 *   is maintained through institutional inertia (the interpretive tradition
 *   cannot formally abolish a Torah commandment) and through its role in
 *   vindicating theological propositions (Torah immutability, eschatological
 *   Temple restoration). The perspectival range spans from mountain
 *   (identity-locked literalists who experience the obligation as
 *   unchangeable divine law) through piton (the institutional and analytical
 *   perspectives recognizing the theatrical maintenance) to snare
 *   (individuals bearing cognitive cost from an unfulfillable obligation).
 *   The constraint is a diagnostic exemplar for commitment-system dynamics: a
 *   kernel (the Torah text commanding sacrifice) whose authority structure
 *   (rabbinic halakhic interpretation) maintains formal obligation despite
 *   functional obsolescence.
 *
 * KEY AGENTS:
 *   - Observant Individual (Literalist Frame): Identity-locked adherent (powerless/identity_locked) — experiences obligation as immutable divine law; exit requires abandoning foundational religious identity
 *   - Rabbinic Authority Structure: Institutional interpreter (institutional/constrained) — maintains obligation's formal status through textual study and liturgical commemoration; constrained by commitment to Torah immutability doctrine
 *   - Temple Reconstruction Movement: Organized activists (organized/mobile) — voluntary participants in preparation for anticipated Temple restoration; see current non-performance as temporary
 *   - Non-Literalist Practitioner: Moderate adherent (moderate/mobile) — interprets obligation as historically contingent or symbolically fulfilled through prayer; mobile exit because this reading is accepted within non-Orthodox streams
 *   - Cognitively Captured Adherent: Identity-locked individual (powerless/identity_locked) — bears psychological cost from maintaining unfulfillable obligation as binding; extraction is cognitive dissonance and eschatological anxiety
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees obligation as legal fossil maintained through institutional inertia and theological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel_flat_control, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel_flat_control, 0.25).
domain_priors:theater_ratio(sacrifice_obligation_kernel_flat_control, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel_flat_control, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel_flat_control, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel_flat_control, "Halakhic Obligation to Perform Temple Sacrifice").
narrative_ontology:topic_domain(sacrifice_obligation_kernel_flat_control, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel_flat_control, '1a6d2915-9dca-4cd0-a170-bc6c5532cc08').
narrative_ontology:cs_kernel_codification('1a6d2915-9dca-4cd0-a170-bc6c5532cc08', fixed_text).
narrative_ontology:cs_authority_grounding('1a6d2915-9dca-4cd0-a170-bc6c5532cc08', lineage).
narrative_ontology:cs_interpretation_layer_present('1a6d2915-9dca-4cd0-a170-bc6c5532cc08').
narrative_ontology:cs_created_at('1a6d2915-9dca-4cd0-a170-bc6c5532cc08', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(sacrifice_obligation_kernel_flat_control, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel_flat_control, halakhic_continuity_claim).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel_flat_control, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel_flat_control, temple_reconstruction_activist).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel_flat_control, literalist_adherent).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel_flat_control, torah_immutability_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel_flat_control, temple_restoration_eschatology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual whose religious identity is constituted through literalist interpretation of Torah commandments. Experiences the sacrifice obligation as binding divine law despite inability to perform it. Bears cognitive and emotional cost (guilt, inadequacy, eschatological anxiety) from maintaining an unfulfillable obligation as active. Cannot exit without abandoning the foundational premise that Torah law is eternal and immutable — exit would require becoming a different kind of Jew or leaving Jewish practice entirely.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, literalist_adherent, payer,
    powerless, immediate, identity_locked, local).

% Halakhic authority structure (rabbinic courts, yeshivot, posek networks) that maintains the obligation's formal legal status through textual study, legal discourse, and liturgical commemoration. Sets the interpretive agenda: determines which readings are legitimate, which adaptations are permissible, and how the obligation's non-performance is framed. Constrained by commitment to Torah immutability doctrine — cannot formally abolish a Torah commandment even when its functional content has been absent for two millennia. Benefits institutionally from maintaining authoritative interpreter role, but this is abstract legitimacy rather than material extraction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, rabbinic_interpretive_body, agenda_setter,
    institutional, biographical, constrained, global).

% Organized groups (Temple Institute, priestly training programs, architectural planning committees) working toward Temple restoration. Benefits from the obligation's formal persistence because it provides theological and legal justification for reconstruction efforts. Collects funding, volunteer labor, and political support on the premise that the obligation remains binding and will be fulfilled when circumstances permit. Mobile exit because participation is voluntary and ideologically driven — activists can leave the movement without social penalty.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, temple_reconstruction_activist, beneficiary,
    organized, generational, mobile, regional).

% Jews who maintain halakhic practice but interpret the sacrifice obligation as historically contingent or symbolically fulfilled through prayer. Neither pays nor collects — the obligation is experienced as adapted rather than suspended, and the adaptation (Rabbinic substitution doctrine) is widely accepted within non-Orthodox streams. Mobile exit because this interpretive stance is normative in Reform, Conservative, and Reconstructionist communities.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, non_literalist_practitioner, observer,
    moderate, biographical, mobile, national).

% Abstract proposition: 'Torah law is eternal and binding across all generations.' The sacrifice obligation's formal persistence vindicates this claim — if the obligation could lapse due to changed circumstances, the immutability doctrine would be undermined. This is a non-agent entry (agent: false) kept for narrative completeness; it is excluded from beneficiary/victim derivation and directionality computation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel_flat_control, halakhic_continuity_claim, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel_flat_control, halakhic_continuity_claim).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original coordination function (Second Temple period, pre-70 CE) was to structure ritual worship, atonement, and festival observance through centralized sacrifice at the Jerusalem Temple. The obligation coordinated pilgrimage, priestly service, agricultural tithes, and the ritual calendar. Today, the coordination function has atrophied: the obligation coordinates only shared liturgical memory, textual study of sacrifice laws, and eschatological hope for Temple restoration. The primary coordination is identity-based (maintaining continuity with Torah commandments) rather than behavioral.
% TRANSFER_FUNCTION: Historically (pre-70 CE): animals, grain, wine, and priestly labor flowed from the Israelite population to the Temple cult. Today: no material transfer occurs. The symbolic transfer is cognitive and emotional investment (study time, liturgical attention, eschatological hope) from adherents to the maintenance of the obligation's formal status. The rabbinic interpretive authority collects abstract legitimacy (the role of authoritative interpreter of divine law) from managing the obligation's persistence, but this is not material extraction.
% ABSENT_VOICES: Voices absent from the halakhic discourse: (1) Jews who left Orthodoxy partly due to cognitive dissonance from unfulfillable obligations — their exit is read as personal failure rather than as evidence the obligation's maintenance imposes cost. (2) Non-Jewish observers who see the obligation's 1,954-year non-performance as evidence it has lapsed — their perspective is excluded by definition from internal halakhic discourse. (3) Historical voices advocating formal suspension or reinterpretation (e.g., early Reform movement) — these were excluded from Orthodox halakhic authority and their readings are not considered legitimate within that system. The unanimity within Orthodox halakhic discourse that the obligation remains binding arises partly because dissenting seats (those who experienced the obligation as obsolete or harmful) exited the conversation.
% DISAPPEARANCE_RATIONALE: If the obligation disappeared overnight (all halakhic texts and liturgical references to sacrifice vanished), the world would rearrange for some and stay unchanged for others. For literalist adherents, the disappearance would be theologically catastrophic — a commandment of divine origin cannot simply vanish, and its absence would undermine Torah immutability. For the rabbinic authority structure, the disappearance would remove a legal category that has been maintained for two millennia, but daily practice would be largely unchanged (since sacrifice is not currently performed). For non-literalist practitioners, the disappearance would formalize what they already believe: the obligation has been adapted, not suspended. For the reconstruction movement, the disappearance would eliminate the theological justification for their efforts. The verdict is contested because different parties dispute whether the obligation is a binding constraint (whose disappearance would matter) or a legal fossil (whose disappearance would merely formalize existing reality).
% FOUNDING_PROBLEM: The founding problem (biblical/Second Temple period) was to structure the Israelite community's relationship with the divine through ritual worship, atonement for sin, and festival observance. Centralized sacrifice at the Jerusalem Temple solved coordination problems: it unified the tribes under a single cult, concentrated priestly authority, synchronized the ritual calendar, and provided a mechanism for atonement and thanksgiving. The obligation was built to solve the problem of how a covenantal community maintains its relationship with a transcendent deity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (structuring ritual worship through Temple sacrifice) is dead in the sense that the Temple has not existed for 1,954 years and the halakhic system has adapted through the Rabbinic substitution doctrine (prayer replaces sacrifice). This status is corroborated by: (1) Historical fact: the Second Temple was destroyed in 70 CE and has not been rebuilt. (2) Halakhic consensus: even within Orthodoxy, prayer is the normative mode of divine service, and no contemporary authority claims sacrifice can or should be performed without the Temple. (3) Comparative evidence: other Torah commandments tied to the Temple (e.g., certain purity laws, priestly tithes) have also lapsed in practice. However, the status is contested by the reconstruction movement, which claims the problem is dormant rather than dead — the obligation persists because the founding problem will return when the Temple is rebuilt. The corroboration comes from outside the beneficiary set (rabbinic authority): historical scholarship, archaeological evidence, and the lived experience of two millennia of Jewish practice without sacrifice.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT INDIVIDUAL / LITERALIST FRAME (MOUNTAIN) — From within an identity frame that takes Torah commandments as immutable divine law, the obligation appears as unchangeable natural fact. The absence of the Temple is experienced as external circumstance preventing fulfillment, not as evidence the obligation has lapsed. Identity-locked because exit would require abandoning the foundational premise that Torah law is eternal and binding. Immediate time horizon because the obligation is experienced as present-tense despite non-performance.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (PITON) — The institutional halakhic system maintains the obligation's formal status while acknowledging it cannot be performed. The constraint persists through textual study, liturgical commemoration, and eschatological hope rather than through actual practice. High theater ratio: the 'obligation' is preserved in legal discourse, prayer, and ritual memory, but the functional content (actual sacrifice) has been absent for 1,954 years. The rabbinic system sees its own maintenance of this obligation as partly inertial — the legal category persists because the interpretive tradition cannot formally abolish a Torah commandment, not because the obligation structures contemporary practice.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TEMPLE RECONSTRUCTION MOVEMENT (SCAFFOLD) — Organized groups (Temple Institute, priestly training programs, architectural planning) see the current non-performance as temporary. The obligation is a coordination mechanism pointing toward a future state: when the Temple is rebuilt, the legal framework will be ready. Sunset logic: the constraint's current form (obligation without performance) is transitional, justified by the anticipated restoration. Mobile exit because participation in reconstruction efforts is voluntary and ideologically driven.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: NON-LITERALIST PRACTITIONER (ROPE) — Jews who maintain halakhic practice but interpret the sacrifice obligation as historically contingent or symbolically fulfilled through prayer (the Rabbinic substitution doctrine: 'our lips replace the bulls'). Experiences the constraint as coordination: the legal tradition provides continuity and shared interpretive framework, but the specific obligation is understood as adapted rather than suspended. Low extraction because this reading is widely accepted within non-Orthodox streams and does not require defending against institutional pressure.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COGNITIVELY CAPTURED ADHERENT (SNARE) — Individual whose identity is fused with literalist halakhic obligation but experiences cognitive dissonance from the obligation's non-performance. Bears psychological cost (guilt, inadequacy, eschatological anxiety) from an obligation that cannot be fulfilled. Identity-locked because the framing ('I am obligated but prevented') is constitutive of religious identity. Extraction is the sustained cognitive and emotional cost of maintaining an unfulfillable obligation as binding. Suppression comes from the interpretive authority's insistence that the obligation remains active despite impossibility of performance.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational view, the sacrifice obligation is a legal fossil: a commandment whose functional content (Temple sacrifice) ceased in 70 CE but whose formal status persists through institutional inertia and theological commitment to Torah immutability. The constraint is maintained theatrically through study, liturgy, and eschatological projection. The analytical classification matches the institutional perspective (both see piton) because the structural reality is the same: high theater ratio, low functional content, persistence through inertia rather than through coordination or extraction. The constraint's primary function today is to vindicate the proposition that Torah law is eternal, not to coordinate actual sacrifice.
constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sacrifice_obligation_kernel_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sacrifice_obligation_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(sacrifice_obligation_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The obligation's primary cost today is cognitive and symbolic rather than material. Most adherents experience the non-performance as normalized (prayer has replaced sacrifice) or as externally prevented (no Temple) rather than as personal failure. The extraction is concentrated on identity-locked literalists who bear cognitive dissonance, but this is a minority experience. The decreasing trajectory (0.30 → 0.15) reflects halakhic adaptation over centuries. Suppression (0.25): Low-moderate. The obligation is maintained through interpretive authority and theological commitment rather than through active enforcement. Individuals can adopt non-literalist readings (prayer as fulfillment) without severe social penalty in most contemporary Jewish communities. The suppression is higher for those in stricter Orthodox contexts where literalist interpretation is normative. Theater ratio (0.85): Very high. The obligation is preserved almost entirely through performative maintenance: textual study of sacrifice laws, liturgical references, architectural planning for hypothetical Temple reconstruction, and eschatological theology. The functional content (actual sacrifice) has been absent for 1,954 years. The increasing trajectory (0.70 → 0.85) reflects the growing gap between formal obligation and actual practice as centuries pass without Temple restoration.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how commitment-system dynamics produce radical perspectival divergence from identical structural data. The literalist adherent sees an immutable divine law (mountain) — the obligation is unchangeable, and only external circumstances prevent fulfillment. The rabbinic institution sees a legal category maintained through inertia and theological necessity (piton) — the obligation persists formally but not functionally. The reconstruction movement sees a transitional coordination mechanism with a sunset (scaffold) — current non-performance is temporary, pointing toward restoration. The non-literalist practitioner sees adapted coordination (rope) — the obligation has been reinterpreted, not suspended. The cognitively captured adherent sees extraction (snare) — bearing cost from an unfulfillable obligation. The analytical observer sees the same piton as the institution: theatrical maintenance of a legal fossil. The gap is not about different facts but about different identity frames and structural positions relative to the interpretive authority. The mountain perspective is particularly diagnostic: it demonstrates identity_locked exit at work — the adherent could physically adopt a non-literalist reading, but doing so would require abandoning the identity frame (Torah as immutable divine law) that constitutes their religious self-concept.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The halakhic_continuity_claim and rabbinic_interpretive_authority are listed as beneficiaries because the obligation's formal persistence vindicates the proposition that Torah law is eternal and maintains the rabbinic system's role as authoritative interpreter of divine commandment. However, these are abstract institutional benefits rather than material extraction — no agent is collecting rents from the obligation's maintenance. The low extractiveness (0.15) reflects this: the constraint's primary function today is symbolic and theological rather than extractive. Victims are not declared because the cognitive cost (borne by identity-locked literalists) is a minority experience and is self-imposed through interpretive choice rather than structurally forced. The constraint is unusual in that its beneficiaries are propositions and institutional authority rather than material actors, and its extraction mechanism is cognitive dissonance rather than resource transfer. This is characteristic of commitment-system constraints where the kernel (Torah text) and authority structure (rabbinic interpretation) are the primary structural features.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that piton is the correct classification from both institutional and analytical perspectives, while other perspectives (mountain, scaffold, rope, snare) are legitimate readings from different structural positions. The mandate (perform Temple sacrifice as commanded in Torah) has outlived its function (actual sacrifice ceased in 70 CE), but the formal obligation persists through institutional inertia and theological commitment. The theater_ratio trajectory (0.70 → 0.85) and the extractiveness trajectory (0.30 → 0.15) together show the piton signature: increasing performative maintenance as functional content atrophies. The constraint is not mislabeled coordination (it coordinates very little today beyond shared liturgical memory) and not pure extraction (the costs are primarily cognitive and symbolic, not material). The mandatrophy is resolved: the obligation is a legal fossil, maintained theatrically, whose primary function is to vindicate theological propositions rather than to coordinate behavior or extract resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_adaptation,
    'Is the sacrifice obligation''s persistence evidence of Torah immutability (the obligation remains binding despite non-performance) or evidence of halakhic adaptation (the obligation has been functionally replaced by prayer and study)?',
    'Theological and historical analysis: Does the Rabbinic substitution doctrine (''our lips replace the bulls'') constitute fulfillment, suspension, or reinterpretation of the original obligation? Comparison with other Torah commandments that ceased to be performed (e.g., Jubilee year, certain purity laws).',
    'If immutability: the mountain perspective from identity-locked adherents is structurally correct, and the obligation is a genuine constraint awaiting changed circumstances. If adaptation: the piton classification is correct, and the obligation''s formal persistence is institutional theater masking functional obsolescence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutability_vs_adaptation, conceptual, 'Whether obligation persistence reflects immutability or adaptation').

omega_variable(
    eschatological_sunset_reality,
    'Is the Temple reconstruction movement''s sunset logic (scaffold perspective) a genuine structural feature — the obligation is transitional, pointing toward restoration — or is it aspirational theology that mistakes eschatological hope for institutional planning?',
    'Empirical tracking: Are Temple reconstruction efforts accumulating resources, political support, and institutional capacity at rates consistent with eventual implementation? Or is the ''sunset'' perpetually deferred, making it functionally equivalent to no sunset?',
    'If genuine sunset: scaffold classification is correct for the reconstruction movement''s perspective. If perpetually deferred: the sunset is itself theatrical, and even the movement''s perspective should classify as piton (maintaining the obligation through performance of preparation rather than through actual progress toward implementation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_sunset_reality, empirical, 'Whether Temple reconstruction sunset is structural or aspirational').

omega_variable(
    cognitive_cost_magnitude,
    'What is the actual psychological and social cost borne by individuals who maintain the obligation as binding despite its non-performance? Is the ''snare'' perspective (identity-locked adherent experiencing extraction through cognitive dissonance) a marginal case or a widespread structural feature?',
    'Ethnographic and psychological research within Orthodox communities: prevalence of guilt, anxiety, or cognitive dissonance related to unfulfillable obligations; correlation with strictness of literalist interpretation.',
    'If widespread and severe: extractiveness should be higher (current 0.15 may underestimate). If marginal: the snare perspective is an edge case, and the dominant experience is either piton (institutional maintenance) or rope (non-literalist adaptation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_cost_magnitude, empirical, 'Magnitude of cognitive cost from unfulfillable obligation').

omega_variable(
    rabbinic_authority_benefit,
    'Does the rabbinic interpretive authority structure benefit from maintaining the sacrifice obligation''s formal status? Is the obligation''s persistence a coordination mechanism (preserving legal continuity) or an extraction mechanism (the authority derives legitimacy from managing an unfulfillable obligation)?',
    'Historical and institutional analysis: Does the obligation''s maintenance concentrate interpretive authority, funding, or social status in rabbinic institutions? Comparison with halakhic domains where obligations were explicitly suspended or adapted.',
    'If extraction: rabbinic_interpretive_authority should be reclassified from beneficiary to primary beneficiary, and extractiveness should be higher. If coordination: the current classification (piton with low extraction) is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_benefit, conceptual, 'Whether rabbinic authority benefits from obligation maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel_flat_control, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_obl_theater_70ce, sacrifice_obligation_kernel_flat_control, theater_ratio, 0, 0.7).
narrative_ontology:measurement(sac_obl_theater_570ce, sacrifice_obligation_kernel_flat_control, theater_ratio, 500, 0.75).
narrative_ontology:measurement(sac_obl_theater_1070ce, sacrifice_obligation_kernel_flat_control, theater_ratio, 1000, 0.8).
narrative_ontology:measurement(sac_obl_theater_1570ce, sacrifice_obligation_kernel_flat_control, theater_ratio, 1500, 0.83).
narrative_ontology:measurement(sac_obl_theater_2024ce, sacrifice_obligation_kernel_flat_control, theater_ratio, 1954, 0.85).

% Extraction over time
narrative_ontology:measurement(sac_obl_extract_70ce, sacrifice_obligation_kernel_flat_control, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sac_obl_extract_570ce, sacrifice_obligation_kernel_flat_control, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(sac_obl_extract_1070ce, sacrifice_obligation_kernel_flat_control, base_extractiveness, 1000, 0.2).
narrative_ontology:measurement(sac_obl_extract_1570ce, sacrifice_obligation_kernel_flat_control, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(sac_obl_extract_2024ce, sacrifice_obligation_kernel_flat_control, base_extractiveness, 1954, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat construction of the sacrifice obligation substrate. The constraint is authored as a single story without decomposition into readings. Contestation appears as perspectival disagreement (mountain vs piton vs scaffold vs rope vs snare across different observer positions) and as omega variables (immutability vs adaptation, eschatological sunset reality, cognitive cost magnitude, rabbinic authority benefit). If this substrate is later decomposed into readings (e.g., literalist reading, substitution reading, eschatological reading), each reading would be a separate constraint story with its own beneficiary/victim structure and its own metrics, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
