% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Authority Cession: Retrospective Snare Exposure (Mistranslation as Extraction Mechanism)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) presents itself as a founding agreement
 *   between the Crown and Māori chiefs, but the English and Māori versions
 *   diverge on a foundational point: the English text claims Māori chiefs
 *   ceded sovereignty to the Crown; the Māori text promises protection of
 *   rangatiratanga (chiefly authority, self-determination). THIS READING
 *   instantiates the retrospective snare exposure: the constraint operates as
 *   pure extraction with a covert mechanism — the chiefs signed a text in
 *   their language that they understood as guaranteeing their retained
 *   authority, while the Crown enforced an English text that claimed they had
 *   surrendered that authority. The extraction became visible only when the
 *   divergence was documented and analyzed retrospectively, revealing that
 *   the institutional arrangements (land confiscation, legislative override,
 *   administrative subordination of Māori governance) operated under the
 *   English cession reading throughout, despite the Māori text's promise of
 *   retained authority. The constraint exhibits all the features of a snare:
 *   high extractiveness (0.68), high suppression (0.72), high theater (0.85),
 *   and a mechanism that persists through institutional enforcement — the
 *   Crown's legal apparatus continuously performs the English reading
 *   regardless of the Māori text. The theater_ratio trajectory shows the
 *   constraint's evolution: at signature (t=0), the theater was minimal — a
 *   straightforward exchange of promises. Post-signature (t=5), theater
 *   increased as the Crown enforced interpretations that contradicted the
 *   Māori text while claiming to honor the treaty. By the generational moment
 *   (t=15), the theater dominates — the treaty is invoked as a legitimating
 *   symbol while the actual mechanisms of extraction (land confiscation,
 *   legislative override, administrative override) operate under the English
 *   reading entirely. The suppression trajectory shows increasing enforcement
 *   intensity: at signature, suppression was moderate (Māori could still
 *   mobilize to challenge); post-signature, it rose as Crown military and
 *   administrative capacity increased; at the generational moment,
 *   suppression is near-total through institutional entrenchment —
 *   challenging Crown legal authority means challenging the entire settled
 *   order of the settler state. This is the retrospective snare diagnosis:
 *   covert at operation time, visible at retrospection time, extraction
 *   mechanism anchored in mistranslation that could only be exposed once the
 *   texts were compared and analyzed.
 *
 * KEY AGENTS:
 *   - Māori signatories and descendants (powerless/trapped): Primary victims bearing full extraction; understood themselves as retaining rangatiratanga but were stripped of authority under the English reading
 *   - Crown land-purchasing apparatus (institutional/arbitrage): Primary beneficiary (arbitrage exit); benefits from efficient land transfer while minimizing violent resistance; experiences the treaty as coordination mechanism
 *   - Colonial settlement authority (organized/constrained): Secondary beneficiary and enforcer; coordinates settlement expansion constrained nominally by treaty; exploits textual divergence to enforce English reading
 *   - Treaty institution as performative artifact (institutional/constrained): The treaty structure itself, increasingly theatrical over time; provides legitimating ceremonial function while administrative machinery operates under English reading
 *   - Analytical observer (analytical/analytical): Risks naturalizing translation difficulties as inevitable rather than strategically exploited
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.68).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.72).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession: Retrospective Snare Exposure (Mistranslation as Extraction Mechanism)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '1c0e9a06-4b3f-4756-a3fd-0302c089afc8').
narrative_ontology:cs_kernel_codification('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', fixed_text).
narrative_ontology:cs_authority_grounding('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', extraction).
narrative_ontology:cs_interpretation_layer_present('1c0e9a06-4b3f-4756-a3fd-0302c089afc8').
narrative_ontology:cs_reading_relation('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', foundational, textual_divergence_is_structural_not_accidental).
narrative_ontology:cs_axiom_status(textual_divergence_is_structural_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', textual_divergence_is_structural_not_accidental, empirically_contingent).
narrative_ontology:cs_axiom('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', foundational, extraction_covertness_enabled_by_mistranslation).
narrative_ontology:cs_axiom_status(extraction_covertness_enabled_by_mistranslation, holdable).
narrative_ontology:cs_axiom_grounding('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', extraction_covertness_enabled_by_mistranslation, empirically_contingent).
narrative_ontology:cs_reference_frame('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', shared_commitment_treaty_framework).
narrative_ontology:cs_drift_state('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', contemporary_textual_analysis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c0e9a06-4b3f-4756-a3fd-0302c089afc8', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_colony_administrative_state).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_rangatiratanga_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MĀORI SIGNATORIES AND DESCENDANTS (SNARE) — Trapped by a legal instrument they could not have understood in the English sovereign cession reading. The text they signed promised protection of rangatiratanga (authority, self-determination); the English version ceded sovereignty itself. No exit mechanism exists post-signature; the land transfer is irreversible and enforced by the settler state's administrative machinery. The extraction is covert at time of operation — visible only retrospectively when the divergence between Māori and English texts becomes legible. Maximum suppression: the Crown's legal apparatus enforces the English reading regardless of Māori comprehension.
constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROWN LAND-PURCHASING APPARATUS (ROPE) — Experiences the treaty as coordination mechanism for legitimate land acquisition. From this perspective, the treaty solves a collective action problem: how to legitimize colonial expansion with minimal violent resistance. The apparatus benefits from efficient land transfer (arbitrage exit option — can exit the treaty framework and acquire land through other means if treaties fail). The extraction flow runs entirely toward the beneficiary, but the apparatus perceives this as fair exchange and recognized authority — no coercion from the apparatus's structural position.
constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL SETTLEMENT AUTHORITY (TANGLED ROPE) — Administrative middle layer that coordinates settlement expansion AND is constrained by treaty obligations (at least in principle). The authority genuinely coordinates resource allocation and land distribution for settlers while also facing formal constraints on unilateral action — the treaty represents real coordination function at this level. However, the English/Māori textual divergence enables the authority to enforce one reading while claiming to honor the other, creating asymmetric extraction over the coordination function. The authority benefits from the ambiguity but is also constrained by it: if the divergence becomes too visible, legitimacy erodes.
constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TREATY INSTITUTION AS PERFORMATIVE SURVIVAL (PITON) — The treaty itself, viewed as an institutional structure, persists largely through theatrical acknowledgment rather than functional operation. The theater_ratio (0.85) reflects that the treaty is invoked as legitimating symbol while its actual binding force on land transfer and authority distribution is minimal — administrative practice operates under the English reading regardless of treaty language. The institution survives because it provides ceremonial continuity (Waitangi Day commemoration, official treaty texts) without materially constraining state action. This is institutional inertia: the treaty persists because replacing it with explicit colonial rule would expose the extraction mechanism, even though the current regime already enforces that rule covertly.
constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint might appear as an immutable feature of colonial encounter: translation gaps and mutual incomprehension are inherent to cross-cultural legal encounters; some asymmetry of understanding is inevitable when oral societies encounter written legal text; the divergence reflects the ontological incommensurability between Māori and European concepts of authority and land. This perspective risks naturalizing what is actually a contingent institutional arrangement enforced by power asymmetry. The engine will identify this as a false summit — the 'inevitable incomprehension' framing naturalizes what is actually a deliberately maintained extraction mechanism.
constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treaty_authority_cession__retrospective_snare_exposure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, TR),
    TR >= 0.70.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68): High. The constraint extracts land, political authority (rangatiratanga), and economic resources from Māori through systematic enforcement of the English cession reading against the Māori promise of retained authority. The extraction is substantial and persistent. However, it is not maximal (0.85+) because the Crown is constrained by having to maintain the pretense of treaty observance — if the divergence became undeniable and the Māori reading was fully acknowledged, Crown legal authority would erode. The measurement trajectory (0.15 → 0.42 → 0.68) shows extraction accumulation: at signature, the extraction was nascent (minimal land had been transferred, authority dynamics were unclear); post-signature (t=5), extraction accelerated as the Crown systematized land confiscation under the English reading; at t=15 (generational moment), extraction had accumulated substantially but remained concealed behind institutional machinery and legal performance. SUPPRESSION (0.72): High. The Crown's legal apparatus enforces the extraction with minimal alternatives: Māori legal challenges to confiscations were rejected on grounds of Crown sovereignty; armed resistance was suppressed militarily; attempts to organize collective action faced Crown administrative suppression; the settler colonial state naturalized Crown supremacy as inevitable. The trajectory (0.50 → 0.65 → 0.72) reflects increasing institutional entrenchment — as the Crown's administrative capacity grew, suppression became more complete. THEATER RATIO (0.85): Very high. The constraint operates through continuous performance of the treaty as a binding and honored commitment while the actual mechanisms (land confiscation, legislative override, administrative subordination of Māori institutions) operate under the English reading entirely. The trajectory (0.25 → 0.55 → 0.85) reflects the growth of ceremonial observance inversely proportional to actual constraint on Crown action — the more the treaty was invoked as a symbol, the less it constrained state action. At t=15, the treaty's theater is near-complete: Waitangi Day commemorations, official treaty texts on government documents, rhetoric of partnership all coexist with persistent refusal to honor the Māori text's reading. CLAIMED TYPE JUSTIFICATION (SNARE): The constraint exhibits all snare markers: (1) high extractiveness targeting a specific victim set (Māori signatories and descendants); (2) high suppression with minimal alternatives; (3) existence that depends on suppressing the alternative reading (if the Māori text's interpretation was enforced, the extraction would reverse); (4) beneficiary that extracts persistent advantage (land, political authority) with mechanisms that would fail if the asymmetry became fully visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The Māori signatories experience it as a snare (they were trapped by a text they thought they understood as guaranteeing retained authority; extraction only became visible as subsequent Crown actions violated that promise). The Crown apparatus experiences it as rope (legitimate coordination for land acquisition). The colonial settlement authority experiences it as tangled_rope (genuine coordination function for settlement plus extraction enabled by textual ambiguity). The treaty institution itself experiences it as piton (ceremonial survival through theatrical invocation despite operational irrelevance). The analytical observer risks the false-summit error of naturalizing mistranslation as inevitable rather than structurally exploited. The perspectival gaps reveal the constraint's actual mechanics: the same institutional arrangements that appear as coordination to the beneficiary appear as systematic extraction to the victim; the same legal instrument that appears as a binding constraint to the Crown appears as a broken promise to Māori because the Crown enforces one reading while claiming the other.
 *
 * DIRECTIONALITY LOGIC:
 *   MĀORI SIGNATORIES (powerless/trapped): d ≈ 0.95 (maximum target). They are victims with no exit — the land transfer is irreversible, legal authority is stripped, and the settler state's monopoly on violence and law prevents escape. The English reading is enforced regardless of the Māori text they signed. This agent experiences maximum effective extraction chi. CROWN APPARATUS (institutional/arbitrage): d ≈ 0.05 (full beneficiary). They can exit the treaty framework and acquire land through other means if needed; they experience the constraint as enabling efficient coordination. The extraction flow runs entirely toward them. This agent experiences negative or minimal chi (benefits outweigh costs). COLONIAL SETTLEMENT AUTHORITY (organized/constrained): d ≈ 0.60 (near-symmetric). The authority is both beneficiary (benefits from land acquisition and settlement coordination) and partially constrained (nominally must respect treaty, though the English reading enables override). The authority benefits from the ambiguity but is also at risk if the divergence becomes fully visible. ANALYTICAL OBSERVER (analytical/analytical): d ≈ 0.72 (analyst as observer of structure). The analyst is not a beneficiary or victim; they perceive the structural asymmetry between the readings. The engine derives canonical d from the analytical power atom, which maps to moderate experienced extraction — the analyst perceives substantial extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This reading explicitly resolves the mandatrophy by documenting how a constraint CAN simultaneously appear as legitimate coordination (the Crown's rope perspective) and systematic extraction (the Māori victims' snare perspective) without either perspective being false. The mandatrophy here is: 'How can the treaty be both a coordination mechanism and an extraction mechanism?' The resolution is: it is both, from different perspectives, BECAUSE the textual divergence itself is the extraction mechanism. The divergence enables the beneficiary to perform coordination while operating extraction — to claim legitimacy through the treaty while systematically exploiting the English reading. The analytic resolution: mandatrophy is not resolved by finding the 'correct' type; it is resolved by recognizing that the constraint operates as a snare PRECISELY BECAUSE its extraction is covert, enabled by institutional arrangements that allow beneficiaries to perceive it as coordination while victims experience it as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_divergence,
    'Was the textual divergence between Māori and English treaty versions produced intentionally (deliberate mistranslation for extraction) or through genuine translation difficulty?',
    'Analysis of translator records, Crown archives, comparative linguistic history; examination of whether similar ambiguities appear in other contemporary treaties or whether this divergence is structurally unique; investigation of whether the Crown deliberately sourced translators known to favor Crown interests.',
    'If intentional: the snare classification is confirmed — extraction was strategically designed. If unintentional: the constraint shifts toward tangled_rope or scaffold (a coordination problem with downstream harm, not deliberate extraction). If mixed (intentional at some points, unintentional at others): the constraint remains snare but the mechanism is partially opaque.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_divergence, empirical, 'Whether mistranslation was deliberate or emergent from translation difficulty').

omega_variable(
    maori_comprehension_actual,
    'What did the Māori signatories actually understand about the cession claim versus rangatiratanga retention claim at the moment of signature?',
    'Analysis of oral history records, Māori genealogies and accounts, contextual statements by signatories and witnesses in the years immediately following signature, comparison with how signatories understood subsequent Crown actions (land confiscation, legislative override), examination of whether Māori behavior post-signature is consistent with understanding a cession or a retained authority claim.',
    'If signatories understood cession: the extraction mechanism was covert (they assented to something they understood as different). If signatories understood retention: their non-resistance post-signature suggests they were actually trapped (unaware they had been stripped of legal authority despite thinking they retained it). If understanding was genuinely indeterminate: the snare mechanism exploited the indeterminacy itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maori_comprehension_actual, empirical, 'Actual Māori understanding of cession claim at moment of signature').

omega_variable(
    rangatiratanga_definition_contest,
    'Does rangatiratanga mean retained tribal authority (as signatories appear to have understood) or subordinate authority constrained by Crown sovereignty (as the Crown''s subsequent actions enforced)?',
    'Linguistic and historical analysis of rangatiratanga usage in pre-treaty and post-treaty Māori documentation; comparison with how Crown legal instruments defined the term; examination of how the Crown enforced or overrode rangatiratanga claims in practice; investigation of whether there is any Māori linguistic or legal tradition that supports the ''subordinate authority under Crown sovereignty'' reading.',
    'If rangatiratanga means retained full authority: the Crown''s legal claim to override it is a pure extraction mechanism (snare confirmed). If rangatiratanga is genuinely ambiguous: the snare operates through deliberate exploitation of semantic ambiguity. If the Crown''s subordinate reading has any textual or traditional basis: the constraint becomes tangled_rope (mixed coordination and extraction rather than pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_definition_contest, conceptual, 'Definition of rangatiratanga: retained authority vs. subordinate authority under Crown sovereignty').

omega_variable(
    treaty_kernelhood_status,
    'Is the Treaty of Waitangi functioning as a genuine kernel (stabilized commitment that grounds legitimacy) or as a false kernel (ceremonial artifact behind which the Crown operates unilaterally)?',
    'Analysis of whether Crown legal decisions actually cite or defer to treaty language; examination of whether Māori have any real institutional leverage to invoke the treaty against Crown action; investigation of whether the treaty functions as a constraint on Crown power or merely as a legitimating symbol; comparison with how the Crown treats other foundational legal documents (Constitution Act, Bill of Rights).',
    'If genuine kernel: the treaty is a commitment system constraint that the Crown actively maintains and interprets (possibly extractively). If false kernel: the treaty is a piton (degraded institution maintained for theater); the actual extraction operates under administrative machinery unconnected to treaty language. If semi-functional: the treaty has kernelhood but subject to Crown interpretive override (tangled_rope structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_kernelhood_status, empirical, 'Whether the Treaty of Waitanui functions as genuine commitment kernel or ceremonial artifact').

omega_variable(
    legislative_override_mechanism,
    'What makes legislative override of treaty obligations possible? Is it the textual divergence itself, Crown institutional supremacy doctrine, Māori political powerlessness, or some combination?',
    'Analysis of specific legislative instruments that overrode treaty claims; examination of constitutional doctrines invoked (parliamentary sovereignty, Crown prerogative); investigation of whether similar legislative overrides occur for other sacred texts (religious doctrine, constitutional commitments); comparison with how other settler colonial states handled comparable treaty obligations.',
    'If divergence drives override: the snare mechanism is specifically linguistic (the English reading enables legal override without apparent violation). If institutional supremacy drives override: the snare mechanism is structural power (the treaty is never binding because the Crown claims interpretive monopoly). If powerlessness drives override: the snare mechanism is political (Māori cannot enforce the treaty they signed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_override_mechanism, empirical, 'Root cause of legislative override capacity: textual divergence, institutional supremacy, or political powerlessness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tac_snare_theater_t0_signature, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tac_snare_theater_t5_post_signature, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 5, 0.55).
narrative_ontology:measurement(tac_snare_theater_t15_accumulation, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 15, 0.85).

% Extraction over time
narrative_ontology:measurement(tac_snare_extract_t0_signature, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tac_snare_extract_t5_post_signature, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tac_snare_extract_t15_accumulation, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tac_snare_suppress_t0_signature, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tac_snare_suppress_t5_post_signature, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(tac_snare_suppress_t15_accumulation, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, settler_land_confiscation__institutional_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, legislative_override__maori_authority).

% DUAL FORMULATION NOTE:
% The Treaty of Waitanui kernel has THREE constraint stories corresponding to THREE readings: (1) crown_cession_reading: constraint from Crown's perspective (rope/coordinate authority); (2) rangatiratanga_retention_reading: constraint from Māori traditional authority perspective (retained framework); (3) treaty_authority_cession__retrospective_snare_exposure (THIS story): constraint from retrospective analytical perspective (snare mechanism exposed through textual divergence). Each reading gets its own constraint story with its own ε value. The retrospective snare exposure reading is downstream of both sibling readings because it exposes the structural asymmetry between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, institutional, 0.05).
constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
