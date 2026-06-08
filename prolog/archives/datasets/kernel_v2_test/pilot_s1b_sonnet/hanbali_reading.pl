% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Textualist Reading of Usul al-Fiqh Method
 *   domain: islamic_jurisprudence/legal_theory/methodological_reading
 *
 * SUMMARY:
 *   The Hanbali textualist reading of usul al-fiqh (Islamic legal
 *   methodology) emerged in the ninth century as a response to Mu'tazili
 *   rationalism and represents one of four canonical Sunni legal
 *   methodologies. This constraint story models the Hanbali reading as one
 *   interpretation of a contested kernel: the proper method for deriving
 *   Islamic law from foundational sources. The Hanbali method privileges
 *   direct textual evidence (Quran and authenticated hadith) over analogical
 *   reasoning (qiyas) and minimizes reliance on weaker hadith, rational
 *   extrapolation, or customary practice ('urf). The principle of sadd
 *   al-dhara'i (blocking means to harm) functions as a precautionary gate:
 *   actions not explicitly forbidden but potentially leading to forbidden
 *   outcomes are suppressed. This textualist restrictiveness coordinates
 *   legitimate concerns about jurisprudential drift while simultaneously
 *   extracting from rationalist and customary legal traditions by
 *   delegitimizing their reasoning methods as suspect innovation (bid'a). The
 *   constraint's dual nature — genuine coordination function (preserving
 *   prophetic precedent) plus asymmetric extraction (suppressing adaptive
 *   capacity) — produces a tangled_rope classification from the analytical
 *   perspective. Historical measurements show moderate initial extraction
 *   (0.35 at founding) increasing to 0.48 in the contemporary period as
 *   textualism became institutionally dominant through Wahhabi state adoption
 *   and modern Salafi reform movements. Theater ratio remains relatively low
 *   (0.38) because Hanbali textualism retains substantial functional content:
 *   scholars genuinely engage hadith authentication and textual
 *   interpretation rather than purely performing compliance. The constraint
 *   is part of a four-reading family (Hanafi, Maliki, Shafi'i, Hanbali) where
 *   each reading instantiates different balances between textual
 *   restrictiveness and adaptive capacity.
 *
 * KEY AGENTS:
 *   - Textualist Scholars: Primary beneficiaries (institutional/arbitrage) — Hanbali 'ulama and modern Salafi scholars who gain authority through strict textualism; experience constraint as coordination protecting against innovation
 *   - Rationalist Jurists: Primary victims (moderate/constrained) — Hanafi and Mu'tazili-influenced scholars whose analogical reasoning is delegitimized; retain scholarly mobility but face career costs
 *   - Customary Legal Practitioners: Secondary victims (powerless/trapped) — Local qadis whose integration of 'urf is reclassified as suspect innovation; cannot exit without abandoning professional identity
 *   - Anti-Innovation Institutions: Beneficiaries (institutional/arbitrage) — Wahhabi religious establishment, Salafi reform movements, textual-purity advocacy organizations
 *   - Adaptive Fiqh Coalition: Organized resistance (organized/constrained) — Contemporary maqasid scholars advocating renewed ijtihad; see textualism as historically-bounded response requiring sunset
 *   - State Legal Systems: Mixed position (institutional/constrained) — Post-colonial shari'ah court systems experiencing coordination (clear rules) and extraction (rigidity prevents legal adaptation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.48).
domain_priors:suppression_score(hanbali_reading, 0.67).
domain_priors:theater_ratio(hanbali_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hanbali_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hanbali_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Textualist Reading of Usul al-Fiqh Method").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/methodological_reading").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, 'c2c7a66c-0d6a-46e5-8bba-66c248d75fde').
narrative_ontology:cs_kernel_codification('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', formalized).
narrative_ontology:cs_authority_grounding('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', lineage).
narrative_ontology:cs_interpretation_layer_present('c2c7a66c-0d6a-46e5-8bba-66c248d75fde').
narrative_ontology:cs_reading_relation('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', hanbali_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', hanbali_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', hanbali_reading__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', foundational, textual_sufficiency_primacy).
narrative_ontology:cs_axiom_status(textual_sufficiency_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', textual_sufficiency_primacy, deontological).
narrative_ontology:cs_axiom('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', foundational, analogical_reasoning_minimization).
narrative_ontology:cs_axiom_status(analogical_reasoning_minimization, holdable).
narrative_ontology:cs_axiom_grounding('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', analogical_reasoning_minimization, conventional).
narrative_ontology:cs_axiom('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', secondary, precautionary_innovation_blocking).
narrative_ontology:cs_axiom_status(precautionary_innovation_blocking, holdable).
narrative_ontology:cs_axiom_grounding('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', precautionary_innovation_blocking, instrumental).
narrative_ontology:cs_reference_frame('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', prophetic_way_completeness).
narrative_ontology:cs_drift_state('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', contemporary_legal_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2c7a66c-0d6a-46e5-8bba-66c248d75fde', '2026-06-08T00:00:00Z').
narrative_ontology:cs_kernel_id(hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, salafi_reform_movements).
narrative_ontology:constraint_beneficiary(hanbali_reading, anti_innovation_institutions).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(hanbali_reading, customary_legal_practitioners).
narrative_ontology:constraint_victim(hanbali_reading, adaptive_jurisprudence_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUSTOMARY LEGAL PRACTITIONER (SNARE) — Local qadis and muftis whose jurisprudence integrated 'urf (custom) are structurally delegitimized by Hanbali textualism. Cannot exit the interpretive framework without abandoning professional identity. Experiences maximum extraction: their legal reasoning is reclassified from valid ijtihad to suspect innovation.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RATIONALIST JURIST (TANGLED ROPE) — Hanafi and Mu'tazili-influenced scholars who prioritize qiyas and maslaha. Constrained by institutional pressure but retain scholarly mobility. Experience mixed coordination (shared commitment to Quran/Sunnah primacy) and extraction (analogical reasoning delegitimized as bid'a risk). Can migrate between madhabs but at career cost.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TEXTUALIST SCHOLAR (ROPE) — Hanbali-trained 'ulama and modern Salafi scholars. Experiences the constraint as coordination: strict textualism protects against jurisprudential drift and preserves prophetic authenticity. Net beneficiary through institutional authority, publishing access, and reform movement leadership. Arbitrage exit via engagement with non-Hanbali institutions.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADAPTIVE FIQH COALITION (SCAFFOLD) — Contemporary scholars advocating maqasid al-shari'ah (objectives of Islamic law) and renewed ijtihad. See Hanbali textualism as a historically-bounded response to ninth-century rationalist excess, now requiring sunset as Muslim communities face novel contexts (bioethics, finance, digital life) where textual silence is substantive. Organized through academic networks and reform institutions.
constraint_indexing:constraint_classification(hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE LEGAL SYSTEM (TANGLED ROPE) — Post-colonial national legal systems incorporating shari'ah courts. Experience coordination (Hanbali textualism provides clear rules) and extraction (rigidity prevents legal adaptation to modern governance needs). Constrained exit: abandoning Islamic law entirely triggers legitimacy crisis; adopting full Hanbali restrictiveness creates governance gridlock.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From analytical distance, Hanbali textualism coordinates legitimate concerns (protection against arbitrary innovation, preservation of prophetic precedent) while extracting from adaptive capacity. The constraint genuinely solves a coordination problem (preventing jurisprudential fragmentation) AND creates asymmetric costs (rationalist and customary legal traditions delegitimized). Claimed type matches computed type.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The Hanbali reading genuinely coordinates protection against arbitrary innovation but extracts from rationalist and customary legal traditions by delegitimizing their reasoning methods. The extraction is not maximal because alternative madhahib persist and Hanbali dominance is regional rather than universal. Suppression (0.67): High. Significant barriers to non-textualist jurisprudence include institutional pressure (Hanbali-dominated religious establishments in Saudi Arabia and Gulf states), social stigma (innovation labels), and educational gatekeeping (textualist training in influential seminaries). Suppression has increased over the measurement interval through Wahhabi state adoption and modern Salafi institutional growth. Theater ratio (0.38): Moderate-low. While some hadith citation is performative (boundary maintenance rather than genuine legal derivation), Hanbali textualism retains substantial functional content. Scholars engage in real hadith authentication, chain-of-transmission analysis, and textual interpretation. The method works as described more often than not, distinguishing it from pure performance. Accessibility collapse (0.15): Low. Alternative madhahib remain accessible — the legal field has not converged on Hanbali textualism as the only viable method. Resistance (0.52): Moderate. Significant organized resistance from Hanafi rationalists, Maliki customary practitioners, and contemporary maqasid scholars. The resistance is not marginal.
 *
 * PERSPECTIVAL GAP:
 *   The Hanbali textualist reading produces a wide perspectival gap. Textualist scholars see coordination (Rope) — strict textual fidelity protects against jurisprudential drift and preserves prophetic authenticity. They are net beneficiaries of the institutional authority the method provides. Customary legal practitioners see extraction (Snare) — their jurisprudence is delegitimized as suspect innovation, and they cannot exit without abandoning professional identity. Rationalist jurists see mixed coordination and extraction (Tangled Rope) — they share commitment to Quran/Sunnah primacy but their analogical reasoning is suppressed. The adaptive fiqh coalition sees a temporary problem (Scaffold) — Hanbali textualism was a legitimate ninth-century response to rationalist excess but now requires sunset as Muslim communities face novel contexts where textual silence is substantive. State legal systems see mixed coordination and extraction (Tangled Rope) — textualism provides clear rules but creates governance gridlock. The analytical observer confirms Tangled Rope: the constraint genuinely solves a coordination problem (preventing arbitrary innovation) while creating asymmetric costs (suppressing adaptive capacity). No perspective is 'wrong' — each sees the structural reality from their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (textualist_scholars, salafi_reform_movements, anti_innovation_institutions): Structural beneficiaries experience low directionality toward the constraint (d near 0.0–0.2). The textualist scholars are institutional-power agents with arbitrage exit options — they can engage non-Hanbali institutions when strategically useful. The engine will derive low effective extraction (negative chi) for these agents: they collect from the constraint rather than paying into it. Victims (rationalist_jurists, customary_legal_practitioners, adaptive_jurisprudence_communities): Structural victims experience high directionality (d near 0.6–1.0). The customary practitioners are powerless/trapped — they cannot exit the interpretive framework without abandoning their professional identity. The rationalist jurists are moderate/constrained — they face career costs for non-textualist reasoning but retain scholarly mobility. The adaptive jurisprudence communities are organized/constrained — they can resist but face institutional pressure. The engine will derive high effective extraction for these agents. State legal systems occupy a mixed position: they are institutional-power agents but constrained exit (abandoning Islamic law triggers legitimacy crisis). The engine will derive moderate extraction — they experience the constraint as both coordination and cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali textualist reading does not exhibit mandatrophy in the classical sense: its mandate (preserving textual fidelity) has not been clearly superseded by changed circumstances, though the adaptive fiqh coalition argues it should be. The constraint's mandate is contested rather than obsolete. The measurements show increasing extraction over time (0.35 → 0.48) and rising suppression (0.50 → 0.67) without corresponding increase in theater ratio (0.25 → 0.38), suggesting the method retains functional content even as institutional dominance amplifies its extractive effects. The scaffold perspective (adaptive fiqh coalition) argues for a sunset based on novel contexts requiring renewed ijtihad, but this is a normative claim about what the mandate should be, not an empirical claim that the founding problem (risk of arbitrary innovation) has disappeared. Mandatrophy_resolved is set to false because the constraint's mandate remains live in the textualist coalition's framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Hanbali reading one interpretation of a shared usul al-fiqh kernel, or does it instantiate a fundamentally different kernel (textualist vs rationalist)?',
    'Comparison of shared foundational commitments across madhahib: do all four Sunni schools accept Quran/Sunnah/ijma''/qiyas as the four roots, or does Hanbali restriction of qiyas constitute rejection of a root rather than restrictive application?',
    'If shared kernel: madhahib are readings (committer axis). If different kernels: madhahib are competing constraint families (observer axis). Affects whether cross-madhhab legal pluralism is internal interpretive diversity or structural incommensurability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether Hanbali method is a reading or a distinct kernel').

omega_variable(
    innovation_boundary_under_determination,
    'Where does legitimate interpretive application (ijtihad) end and blameworthy innovation (bid''a) begin under Hanbali textualism?',
    'Historical analysis of Hanbali fatwas: what proportion of contested cases were resolved by direct textual evidence vs weak hadith vs necessity exceptions? If weak hadith use exceeds 30%, the bright line blurs.',
    'If boundary is determinate: textualism is a stable coordination mechanism. If boundary is under-determined: textualism becomes an extraction mechanism where ''innovation'' labels are strategically deployed against rivals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_boundary_under_determination, empirical, 'Empirical determinacy of the bid''a boundary').

omega_variable(
    sadd_al_dharai_scope_creep,
    'Does the precautionary principle of sadd al-dhara''i (blocking means to harm) expand Hanbali restrictiveness beyond textual fidelity into speculative suppression?',
    'Doctrinal analysis: what counts as a ''means'' (dhari''ah) vs a substantive act? If the category expands to include second-order social consequences, sadd al-dhara''i becomes a cover for suppressing innovations the textualist coalition dislikes, not a textual fidelity principle.',
    'If scope is textually bounded: sadd al-dhara''i is coordination. If scope creeps: it is extraction disguised as precaution. Determines whether claimed_type (tangled_rope) or snare is structurally accurate from analytical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dharai_scope_creep, conceptual, 'Whether sadd al-dhara''i scope is textually stable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_tr_t0, hanbali_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanbali_tr_t4, hanbali_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(hanbali_tr_t7, hanbali_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(hanbali_tr_t10, hanbali_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(hanbali_be_t0, hanbali_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hanbali_be_t4, hanbali_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(hanbali_be_t7, hanbali_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(hanbali_be_t10, hanbali_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_su_t0, hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hanbali_su_t4, hanbali_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(hanbali_su_t7, hanbali_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(hanbali_su_t10, hanbali_reading, suppression_requirement, 10, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanbali textualist reading is one of four sibling constraints in the usul al-fiqh kernel family. Each reading has its own ε value reflecting its balance between textual restrictiveness and adaptive capacity. The Hanbali reading has the highest suppression (0.67) and moderate-high extraction (0.48) due to its restrictive scope. The network edges capture that Hanbali textualism sets a standard other madhahib respond to (influences) without foreclosing them (all four readings coexist as live traditions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
