% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Command (Deuteronomy 7) – Durable Separation Reading
 *   domain: biblical_hermeneutics/religious_ethics/commitment_systems
 *
 * SUMMARY:
 *   The herem command in Deuteronomy 7 encodes a divinely mandated separation
 *   constraint that operates across multiple registers: territorial
 *   (displacement of non-covenant outsiders), marital (prohibition of
 *   intermarriage), and categorical (designation of entire ethnic/religious
 *   populations as contaminating and inherently incompatible with covenant
 *   membership). In the durable separation reading, this mandate is
 *   understood as timeless—a foundational principle of covenant identity that
 *   applies universally to all non-covenant outsiders across all historical
 *   periods, not superseded by later law or recontextualized through
 *   allegory. The constraint exhibits classic snare and tangled rope
 *   dynamics: it extracts autonomy (especially sexual and marital autonomy
 *   from covenant members), suppresses alternatives (intermarriage is
 *   categorically prohibited and enforced through community violence), and
 *   legitimates large-scale violence through divine-command framing.
 *   Simultaneously, it solves a genuine coordination problem (maintaining
 *   group boundary and preventing assimilation) for the priestly authority
 *   and covenant institutional continuity perspectives. The measurement
 *   trajectory shows increasing extractiveness over the interval (0.55→0.68)
 *   as later interpretive tradition layers accretions onto the original text,
 *   and rising theater ratio (0.30→0.51) as contemporary enforcement capacity
 *   decays while interpretive emphasis persists. This constraint instantiates
 *   one reading of a contested kernel: the same text (Deuteronomy 7's herem
 *   mandate) is read as durable/universal separation by this reading, as
 *   contextually superseded by later law in a sibling reading, and as
 *   allegorical (applying to spiritual combat, not literal intermarriage) in
 *   a third reading. The committer-axis difference between readings is not
 *   disagreement about facts but divergent normative decisions about how to
 *   bind the authority of ancient law to contemporary ethical frameworks.
 *
 * KEY AGENTS:
 *   - Non-covenant outsiders: Primary victim (powerless/trapped) — designated as categorically contaminating; face violence mandate and territorial displacement with no legitimate exit except assimilation (identity death) or conversion (accepts covenant authority).
 *   - Covenant members tempted to intermarry: Secondary victim (moderate/constrained) — face death penalty or excommunication for violating separation mandate; sexual autonomy is extracted through categorical prohibition.
 *   - Women absorbed through intermarriage: Tertiary victim (moderate/identity_locked) — structurally mobile (marriage offers material security and settlement) but identity-locked by marital assimilation into covenant community; bear full cost of cultural erasure and abandonment of natal kinship.
 *   - Priestly authority / enforcement coalition: Primary beneficiary (organized/constrained) — solve genuine coordination problem (preventing assimilation) while extracting enforcement labor, monopolizing marriage authorization, and concentrating institutional authority over boundary maintenance.
 *   - Covenant institutional continuity: Secondary beneficiary (institutional/arbitrage) — the abstract covenant structure benefits from durable boundary maintenance; frames constraint as coordination rather than oppression.
 *   - Contemporary rabbinic establishment: Tertiary actor (institutional/arbitrage) — maintains the reading through interpretive tradition despite low enforcement capacity; constraint has degraded to piton (theater outweighs function).
 *   - Analytical observer: Observational position (analytical/analytical) — risks naturalizing a contingent institutional choice as a timeless principle of group identity; may commit false summit error by treating the theological framing as establishing natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.68).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.72).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, snare).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command (Deuteronomy 7) – Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics/commitment_systems").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '9f239926-54e7-432c-9601-90801bd0e2fe').
narrative_ontology:cs_kernel_codification('9f239926-54e7-432c-9601-90801bd0e2fe', formalized).
narrative_ontology:cs_authority_grounding('9f239926-54e7-432c-9601-90801bd0e2fe', lineage).
narrative_ontology:cs_interpretation_layer_present('9f239926-54e7-432c-9601-90801bd0e2fe').
narrative_ontology:cs_reading_relation('9f239926-54e7-432c-9601-90801bd0e2fe', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f239926-54e7-432c-9601-90801bd0e2fe', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('9f239926-54e7-432c-9601-90801bd0e2fe', foundational, herem_temporal_universality).
narrative_ontology:cs_axiom_status(herem_temporal_universality, holdable).
narrative_ontology:cs_axiom_grounding('9f239926-54e7-432c-9601-90801bd0e2fe', herem_temporal_universality, theological).
narrative_ontology:cs_axiom('9f239926-54e7-432c-9601-90801bd0e2fe', foundational, ethnic_categorical_contamination).
narrative_ontology:cs_axiom_status(ethnic_categorical_contamination, holdable).
narrative_ontology:cs_axiom_grounding('9f239926-54e7-432c-9601-90801bd0e2fe', ethnic_categorical_contamination, theological).
narrative_ontology:cs_created_at('9f239926-54e7-432c-9601-90801bd0e2fe', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_purity).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, priestly_authority_enforcement).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_agents).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, women_absorbed_through_marriage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-COVENANT OUTSIDER (SNARE) — Designated as categorically contaminating by divine mandate. No exit except assimilation (identity death) or violence. The outsider faces maximal extraction: presence itself is criminalized; intermarriage with community members is forbidden; territorial displacement is mandated. Suppression operates through categorical dehumanization (divine command legitimates violence as obedience, not conquest).
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ABSORBED WOMAN (SNARE) — Structurally mobile (can marry into covenant community) but identity-locked by marital assimilation: she must abandon natal kinship, religious identity, and material autonomy. The 'gentleness' of absorption (marriage rather than violence) masks extraction: she bears full cost of cultural erasure, her children are claimed by the conquering covenant, and refusal is not a live option within patriarchal structures. Suppression operates through family structure and gender hierarchy.
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: COVENANT MEMBER TEMPTED TO INTERMARRY (SNARE) — Faces severe penalty (death or excommunication) for violating the separation mandate. Exit from the constraint (marrying outside) is materially possible (forbidden fruit is still available) but suppression is extreme: religious authority, community enforcement, economic integration all converge to prevent exit. The constraint extracts sexual autonomy from this agent through categorical prohibition.
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRIESTLY ENFORCEMENT COALITION (TANGLED ROPE) — Organized actors (priests, tribal elders, community enforcement mechanisms) experience the herem command as both coordination and extraction. The constraint solves a genuine coordination problem: maintaining cultural-religious boundary in a context of inter-settlement intermarriage and assimilation risk (real historical concern). But it also provides extractive benefits: priests monopolize marriage authorization and legitimacy certification, enforcement labor is extracted from the covenant community, and purity authority becomes institutional capital. Active enforcement is required; the coordination function is genuine (preventing group dissolution) but deeply intertwined with extraction (legitimating violence, concentrating priestly authority, enabling confiscation of outsider property).
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COVENANT INSTITUTIONAL CONTINUITY (ROPE) — The abstract institutional framework (the covenant itself as a perpetual structure) benefits from the durable separation mandate. The herem command is framed as coordination: maintaining the boundaries that make the covenant community coherent and transmissible across generations. From this perspective, the constraint is not extractive but protective—defending group survival against assimilation. This perspective has high institutional power and arbitrage (can reframe the constraint as mutual-benefit coordination); the constraint appears as enabling legitimate group identity rather than oppression.
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: CONTEMPORARY RABBINIC ESTABLISHMENT (PITON) — The durable separation reading persists in interpretive tradition despite being substantially disconnected from function. Modern rabbinic authority cannot enforce territorial exclusion, cannot prevent intermarriage through violence, and does not claim these powers—yet the reading persists as textual-exegetical authority. Theater ratio is moderate-high: the energy spent debating whether Dt 7 mandates literal genocide or merely symbolic separation far exceeds the constraint's actual enforcement capacity. The institutional reading has become largely inertial—maintained for continuity with tradition rather than functional authority.
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal analytical position, some form of ethnic/religious boundary maintenance is framed as inherent to group identity itself—an immutable feature of how bounded communities constitute themselves. This perspective risks naturalizing the durable separation mandate as a timeless feature of human social organization. The engine's false summit detection will flag this: the 'inherent to group identity' framing naturalizes what is actually a contingent institutional choice grounded in specific historical conditions (Iron Age settlement competition, assimilation risk, priestly authority consolidation).
constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(herem_command_dt7__durable_separation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, TR),
    TR >= 0.70.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The durable separation reading encodes extraction from multiple victim sets simultaneously. Non-covenant outsiders face violence mandate and territorial displacement (maximal extraction). Covenant members face sexual autonomy extraction (marital prohibition with enforcement). Women absorbed through intermarriage face cultural and identity erasure. The extractiveness is not moderate because the constraint operates through multiple reinforcing channels: divine-command framing (legitimates violence as obedience), patriarchal kinship authority (suppresses women's marital choice), and territorial consolidation (converts extraction into material gain for beneficiary groups). Suppression (0.72): Very high. Multiple suppression mechanisms converge: (1) divine-command framing prevents exit by claiming illegitimacy of refusal ('disobeying God'); (2) community enforcement (violence, excommunication, social death) makes exit materially catastrophic; (3) patriarchal structure prevents women from exercising exit even when structurally possible; (4) categorical dehumanization of outsiders removes intermarriage as a live option by making it metaphysically unthinkable (the outsider is inherently incompatible). Theater ratio (0.51): Moderate and rising. In the historical period of active enforcement (Iron Age settlement and consolidation), theater was low—the constraint operated through actual violence and territorial control. Contemporary theater is higher: modern rabbinic establishment cannot enforce territorial exclusion or violence, yet the durable separation reading persists as textual authority. The measurement trajectory shows accumulation of interpretive theater: later traditions layered speculative elaborations (symbolic meanings, extended applications) onto the original command, increasing the ratio of exegetical energy to enforcement capacity. Claimed type (Snare): The base configuration is snare: high extractiveness, high suppression, minimal coordination function perceived by victims. The priestly/organized perspective (tangled_rope) and institutional continuity perspective (rope) perceive coordination, but these are beneficiary perspectives—their low experienced extraction is a consequence of being beneficiaries, not evidence that the constraint's fundamental structure is coordination rather than extraction.
 *
 * PERSPECTIVAL GAP:
 *   The durable separation reading generates extreme perspectival divergence. The non-covenant outsider and covenant-tempted-intermarrier perceive pure snare: categorical prohibition with violent enforcement and no legitimate exit. The woman absorbed through intermarriage perceives snare with internalized suppression (identity_locked)—the constraint appears inevitable because her identity becomes constituted through marriage assimilation, even though structural alternatives existed beforehand. The priestly/enforcement coalition perceives tangled_rope: they solve a genuine coordination problem (preventing assimilation) while extracting enforcement authority and institutional capital. The covenant institutional perspective perceives rope: boundary maintenance as mutual-benefit coordination that enables group survival. The contemporary rabbinic establishment perceives piton: the constraint persists through textual authority and interpretive tradition despite near-zero enforcement capacity. The analytical observer risks perceiving mountain: treating ethnic/religious boundary maintenance as a timeless, universal feature of group identity. This perspectival spread (snare→snare→snare→tangled_rope→rope→piton→mountain) demonstrates how the same constraint generates radically different classifications depending on observer position. The gap is not measurement noise but structural—beneficiaries and enforcing authorities genuinely experience coordination benefits that victims do not, and the analytical observer's civilizational timeframe risks naturalizing what is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural relationship to the extraction flow. Non-covenant outsiders: d ≈ 0.98 (full target; victim + trapped exit = maximum extraction experienced). Covenant members tempted to intermarry: d ≈ 0.85 (strong target; victim + constrained exit = high extraction). Absorbed women: d ≈ 0.80 (strong target; victim + identity_locked exit = high extraction with internalized suppression). Priestly enforcement coalition: d ≈ 0.35 (partial beneficiary; some victim exposure through enforcement burden + constrained exit, but primary beneficiary status dominates). Covenant institutional: d ≈ 0.15 (beneficiary; arbitrage exit available; views constraint as enabler rather than limitation). Contemporary rabbinic: d ≈ 0.20 (beneficiary through interpretive authority; arbitrage exit through textual reinterpretation; low experienced extraction). Analytical observer: d ≈ 0.72 (observational position; canonical fallback produces moderate extraction experience, but risks committing false summit error by naturalizing the constraint). The engine will compute χ = ε × f(d) × σ(S) for each perspective, modulating the raw extractiveness by directionality and scope. Victims with high d and local/regional scope will show elevated χ; beneficiaries with low d and global scope will show suppressed χ. The systematic variation in experienced χ across perspectives reveals the constraint's structure: asymmetric extraction concentrated on those with highest d (trapped and identity_locked agents) and lowest power (powerless, constrained agents).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via committer-frame analysis. The constraint's apparent paradox—it is simultaneously pure extraction (snare), legitimate coordination (rope), degraded theater (piton), and natural law (mountain)—is resolved by recognizing that these are not contradictory classifications but perspectival readings generated by different structural positions within the constraint. The mandatrophy does not require discovering that 'one reading is right and the others are wrong.' Rather, it requires recognizing that all the readings are structurally coherent from their respective positions. The beneficiary genuinely benefits from coordination; the victim genuinely experiences extraction; the degraded institution genuinely maintains theater; the natural-law observer genuinely risks naturalizing contingency. The resolution is not to harmonize all perspectives but to map the presheaf of classifications over the observation site (the different (P,T,E,S) tuples) and identify which perspectives commit epistemic errors (false summit risk for the mountain perspective) versus which accurately perceive their structural position. The durable separation reading is a committer-axis choice—one reading of a contested kernel. The sibling readings (contextual_supersession_reading, allegorical_displacement_reading) would generate different perspectival presheaves because they reframe the constraint's applicability and binding force. The mandatrophy is resolved by accepting that the reading choice is not empirically determined but rather reflects a normative decision about how to bind ancient textual authority to contemporary ethical frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_legitimation_mechanism,
    'Is the herem mandate''s violence-legitimating force a consequence of the divine command claim itself, or a contingent use of religious authority?',
    'Historical analysis of how similar violence mandates function in other religious traditions; comparison of rhetorical force when divine command framing vs. pragmatic justification is used; examination of whether violence would be classified differently (e.g., as conquest rather than obedience) without the theological framing.',
    'If inherent to divine command: the constraint''s suppression derives from theological structure. If contingent: the divine command is a cover story for political violence, and the constraint should be reclassified as institutional extraction with theological theater. Classification may shift to higher snare severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_legitimation_mechanism, conceptual, 'Whether violence legitimation is essential to divine-command framing or contingent use').

omega_variable(
    assimilation_risk_empirical_baseline,
    'What was the actual historical risk of ethnic/religious assimilation without the herem mandate? Were intermarriage rates, conversion patterns, and cultural persistence tracking toward group dissolution?',
    'Archaeological and textual evidence on settlement patterns, intermarriage prevalence, and religious practice continuity in periods with/without intensive separation enforcement. Comparison with other Iron Age groups that maintained identity without explicit violence mandates.',
    'If assimilation risk was genuine and severe: the coordination function of the constraint is real, and the tangled_rope classification for enforcement coalition is justified. If assimilation risk was overstated: the violence mandate is extractive coercion beyond what protection-of-group-identity would warrant, strengthening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_risk_empirical_baseline, empirical, 'Historical evidence on assimilation risk without separation enforcement').

omega_variable(
    women_absorption_coercion_vs_opportunity,
    'For women absorbed through intermarriage, does the constraint represent pure coercion (identity erasure imposed against preference) or does it offer material opportunity (access to settled agricultural life, security through marriage) that some agents would rationally choose?',
    'Comparative analysis of women''s material conditions in nomadic vs. settled contexts; oral histories or textual traces of women''s agency in intermarriage decisions; examination of whether prohibition required enforcement indicates women actively sought intermarriage (suggesting the constraint suppresses their preference) vs. whether the prohibition was largely unopposed (suggesting women had limited stakes or actual preference for purity).',
    'If pure coercion: the woman''s perspective is maximally extractive (identity_locked snare). If mixed coercion/opportunity: the classification may shift toward tangled_rope (genuine gains alongside extraction). If women had material incentive to resist: the high suppression value is justified; if they were indifferent, suppression might be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_absorption_coercion_vs_opportunity, empirical, 'Whether women''s intermarriage absorption was coerced or offered material opportunity').

omega_variable(
    reading_naturalization_of_contingency,
    'Is the durable separation reading''s framing of herem as a timeless, universal principle a result of reading Dt 7 literally, or does it involve importing later interpretive traditions that were not native to the original text?',
    'Philological analysis comparing the durable separation reading''s treatment of Dt 7 with how the same textual material is handled in contextual_supersession_reading and allegorical_displacement_reading. Identification of which interpretive layers (medieval rabbinic, Reformation Protestant, modern fundamentalist) contributed the ''timeless'' framing vs. which emerged from the text itself.',
    'If durable separation is a faithful reading of Dt 7: the constraint''s ε value reflects the text''s own extractiveness. If durable separation imports later tradition: the reading itself is a committer-axis choice, and the false summit risk is even higher—the analytical observer is naturalizing not just political violence but a particular hermeneutic tradition''s decision to lock in a violent reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_naturalization_of_contingency, conceptual, 'Whether durable separation reading naturalized contingent interpretive tradition').

omega_variable(
    contemporary_covenant_member_experience,
    'For modern covenant community members, is the durable separation mandate experientially salient (does it constrain felt autonomy) or has it degraded to ceremonial theater disconnected from actual marriage and kinship decisions?',
    'Survey of intermarriage patterns in contemporary faith communities claiming durable separation readings; analysis of enforcement capacity and actual penalties for interfaith marriage; examination of whether the mandate functions as an internalized norm (identity_locked suppression) or as ineffectual doctrine (piton theater).',
    'If salient: the suppression value is justified; the constraint actively constrains biological reproduction and family formation. If theater: the piton perspective is dominant; the constraint''s actual extractiveness is lower than the base properties suggest, and suppression operates primarily through guilt/identity rather than material barrier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_covenant_member_experience, empirical, 'Salience of durable separation mandate in contemporary covenant community practice').

omega_variable(
    false_summit_detection_naturalization,
    'Is this constraint a genuine natural law of group boundary maintenance, or is it a constructed institutional mandate that benefits identifiable actors (priestly authority, patriarchal control over women''s reproduction, territorial consolidation) and uses divine-command framing to naturalize political extraction?',
    'Comparison with constraints in other religious traditions that maintain group identity without explicit violence mandates; examination of which actors benefit from the constraint; analysis of whether the ''divine mandate'' framing changes the constraint''s classification if removed. If the constraint appears snare/tangled_rope without the theological framing, the FSM detection fires—the theological framing is naturalizing a political constraint.',
    'If natural law: mountain classification is appropriate. If constructed: the constraint is a false summit, and the engine will reclassify it via the FSM signature chain. The analytical observer (mountain perspective) is then identified as committing the naturalization error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_naturalization, conceptual, 'Whether naturalization via divine command is legitimate or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_sep_theater_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(herem_sep_theater_t5, herem_command_dt7__durable_separation_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(herem_sep_theater_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(herem_sep_extractiveness_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(herem_sep_extractiveness_t5, herem_command_dt7__durable_separation_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(herem_sep_extractiveness_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(herem_sep_suppression_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(herem_sep_suppression_t5, herem_command_dt7__durable_separation_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(herem_sep_suppression_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, allegorical_displacement_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, patriarchal_kinship_authority_intermarriage_control).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, territorial_consolidation_displacement_extraction).

% DUAL FORMULATION NOTE:
% The herem command text (Dt 7) is a single kernel that generates three structurally distinct constraints through different readings. The durable separation reading (this constraint, ε≈0.68, snare-dominant) emphasizes temporal universality and ethnic categoricality. The contextual supersession reading (sibling, ε≈0.35, rope-dominated) emphasizes historical boundedness and supersession by later law. The allegorical displacement reading (sibling, ε≈0.42, tangled rope) emphasizes spiritual referent and internal application. These three constraints are linked by shared commitment to the same textual kernel but diverge in their ε values, victim/beneficiary structures, and interpretive authority. Downstream constraints (patriarchal kinship control, territorial consolidation) are instantiated BY this reading—they represent mechanisms through which the durable separation reading is enforced. The network reveals that what appears as a single textual claim actually decomposes into multiple distinct constraints depending on hermeneutic choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
