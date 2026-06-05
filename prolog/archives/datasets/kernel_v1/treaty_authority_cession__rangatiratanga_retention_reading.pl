% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty Authority Cession (Rangatiratanga Retention Reading)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is a kernel — a stabilized commitment whose
 *   meaning is contested across multiple readings. This constraint story
 *   instantiates the rangatiratanga_retention_reading: the interpretation in
 *   which (1) the Māori text controls via contra proferentem; (2) kāwanatanga
 *   (governance, rule-making) was ceded to the Crown subject to ongoing
 *   consent from hapū; (3) tino rangatiratanga (authority, chieftainship,
 *   self-determination) was explicitly retained by Māori; and (4) the Treaty
 *   established a binding partnership requiring negotiated authority
 *   exercise, not a one-way transfer of sovereignty. Under this reading, the
 *   Crown's unilateral expansion of authority without hapū consent functions
 *   as a constraint — a hybrid of coordination (the partnership framework)
 *   and extraction (the enforcement asymmetry). The measurement trajectory
 *   shows how the constraint's character changed over 186 years: from low
 *   theater/high extraction (1840–1900, snare) through consolidation of
 *   suppression (1890–1940, snare persistence) to judicial restoration of the
 *   partnership reading (1975+, tangled rope). The theater ratio rises over
 *   time as the constraint becomes increasingly legalized, bureaucratized,
 *   and performative through settlement processes and co-governance protocols
 *   — the rise in theater reflects the scaffold phase, in which institutional
 *   structures emerge to operationalize the partnership reading.
 *
 * KEY AGENTS:
 *   - Iwi and Hapū Collectives: Organized beneficiaries (constrained exit) — hold authority under tino rangatiratanga; experience partnership as both coordination (co-governance benefits) and extraction (limited veto power, financially inadequate settlements)
 *   - Crown (Unilateralist Institutional Legacy): Institutional beneficiary (arbitrage exit) — historically claimed unilateral kāwanatanga; now constrained by judicial partnership reading but retains residual authority and resource control
 *   - Māori Land and Resource Interests (Historical): Primary victim (trapped exit) — could not resist Crown land alienation during 1840–1975 period; subject to legal incapacity doctrine and purchasing regimes that violated tino rangatiratanga retention
 *   - Waitangi Tribunal and Judiciary: Organized institutional actors (analytical/institutional) — recognized and restored the partnership reading; gatekeeping authority over Treaty interpretation and settlement parameters
 *   - Treaty Settlement Apparatus: Institutional framework builders (institutional/arbitrage) — scaffold structures (Ngāi Tahu settlement, Tainui co-management, co-governance protocols) that operationalize partnership reading while managing residual Crown authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.52).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.68).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty Authority Cession (Rangatiratanga Retention Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '43230ac7-45a8-4ea6-bd70-fa8386974bd4').
narrative_ontology:cs_kernel_codification('43230ac7-45a8-4ea6-bd70-fa8386974bd4', fixed_text).
narrative_ontology:cs_authority_grounding('43230ac7-45a8-4ea6-bd70-fa8386974bd4', lineage).
narrative_ontology:cs_interpretation_layer_present('43230ac7-45a8-4ea6-bd70-fa8386974bd4').
narrative_ontology:cs_reading_relation('43230ac7-45a8-4ea6-bd70-fa8386974bd4', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('43230ac7-45a8-4ea6-bd70-fa8386974bd4', biculturalism_reading, coexists_with).
narrative_ontology:cs_axiom('43230ac7-45a8-4ea6-bd70-fa8386974bd4', foundational, maori_text_controls_via_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_controls_via_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('43230ac7-45a8-4ea6-bd70-fa8386974bd4', maori_text_controls_via_contra_proferentem, conventional).
narrative_ontology:cs_axiom('43230ac7-45a8-4ea6-bd70-fa8386974bd4', foundational, tino_rangatiratanga_explicit_retention_binds_crown).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_explicit_retention_binds_crown, holdable).
narrative_ontology:cs_axiom_grounding('43230ac7-45a8-4ea6-bd70-fa8386974bd4', tino_rangatiratanga_explicit_retention_binds_crown, deontological).
narrative_ontology:cs_reference_frame('43230ac7-45a8-4ea6-bd70-fa8386974bd4', treaty_partnership_requiring_consent).
narrative_ontology:cs_drift_state('43230ac7-45a8-4ea6-bd70-fa8386974bd4', contemporary_2020s, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('43230ac7-45a8-4ea6-bd70-fa8386974bd4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, iwi_hapu_collectives).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, indigenous_self_determination_advocates).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, crown_unilateral_authority).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, settler_land_acquisition_absolute_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IWI/HAPŪ COLLECTIVE (ROPE) — Under this reading, the treaty establishes a binding partnership in which Crown authority over governance (kāwanatanga) is conditional on ongoing consent from hapū. Tino rangatiratanga (authority/chieftainship) remains vested in whānau and hapū. The constraint functions as coordination: the Crown's legitimate exercise of kāwanatanga requires consent protocols, co-governance structures, and hapū veto on matters affecting ancestral lands and resources. This is experienced as genuine partnership coordination, not extraction, because the mechanism creates reciprocal obligation rather than asymmetric cost.
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: MĀORI LAND INTERESTS / RESOURCE RIGHTS (SNARE) — Retrospectively, during the century between Treaty signing (1840) and its judicial recognition (1970s+), the constraint operated as a snare: the Crown exercised kāwanatanga without hapū consent, systematically alienating land through purchasing regimes that violated tino rangatiratanga retention, while the translation asymmetry (English 'sovereignty' vs Māori 'kāwanatanga') concealed the extraction in law. Powerless Māori groups could not exit; suppression came through legal capacity denial, purchase pressure, and exclusion from decision-making. This is the historical moment when the partnership reading was foreclosed by unilateral Crown action.
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MODERN IWI LEADERSHIP (TANGLED ROPE) — Contemporary iwi negotiating treaty settlements face a genuine hybrid: the partnership reading is now judicially recognized (Waitangi Tribunal, Court of Appeal interpretations), creating a coordination function (co-governance arrangements, Treaty settlement frameworks), yet the constraint still extracts through power asymmetries in negotiation, financial inadequacy of settlements, and the Crown's residual authority to define the scope of redress. Iwi leadership experience both the benefit of recognized partnership and the cost of constrained negotiating power.
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TREATY SETTLEMENT / CO-GOVERNANCE APPARATUS (SCAFFOLD) — The formalized treaty settlement process (Waitangi Tribunal, Office of Treaty Settlements, co-governance frameworks, resource consent protocols) operates as a temporary scaffold: it provides institutional pathways for actualizing the partnership reading while the broader Crown/Iwi relationship undergoes fundamental re-constitution. Theater is moderate (formal protocols, advisory structures, compensation formulas) but the sunset logic is embedded: as co-governance becomes normalized and the partnership reading matures institutionally, the scaffold's temporary structures (dedicated settlement agencies, transitional advisory bodies) should dissolve into permanent partnership institutions. Estimated sunset: 30-50 years for institutional maturation.
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CROWN'S UNILATERAL SOVEREIGNTY FRAMING (PITON) — The historical Crown position (1840–1970s) that treated the Treaty as a one-way cession of 'sovereignty' to the Crown, with no retained Māori authority, persists as institutional inertia in some Crown agency practices despite being judicially foreclosed. This perspective sees the Crown's claimed unilateral authority as a degraded and increasingly ceremonial claim — maintained through bureaucratic practice rather than legal force. The piton classification reflects high theater (sovereignty doctrine continues to organize Crown hierarchy charts) coupled with low actual force (the partnership reading is now the binding legal framework).
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN NATURAL LAW VIEW) — From a civilizational/universal perspective, the transfer of 'sovereignty' from a Māori polity to the British Crown might be framed as an immutable, natural legal consequence of treaty cession — once sovereignty is ceded, it is ceded; the partnership reading would be analytically impossible. However, this perspective is revealed as a false summit: the mountain classification naturalizes what is actually a contestable reading of equivocal treaty language (the English text's 'sovereignty' versus the Māori text's 'kāwanatanga'). The structural data shows beneficiaries (who? Crown unilateralists) and victims (powerless Māori groups during land alienation). The analytical observer must declare this as a reading choice, not a natural law.
constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treaty_authority_cession__rangatiratanga_retention_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, TR),
    TR >= 0.70.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, contemporary): Moderate-high. The partnership reading is now judicially recognized, so direct extraction via unilateral Crown action is legally foreclosed. However, modern negotiations over treaty settlements, resource consent, and co-governance authority continue to extract through power asymmetries: the Crown controls settlement funding, retains authority to define scope of redress, and maintains superior institutional capacity in protracted negotiations. The extractiveness is lower than the historical snare (0.92) but higher than pure rope (≤0.35) because asymmetric power persists beneath the partnership framework. Suppression (0.68, contemporary): Moderate-high. The judicial recognition of the partnership reading has reduced suppression relative to the 1840–1975 snare (0.95), but substantial suppression remains through: (a) historical dispossession (land base adequate for modern partnership is constrained); (b) capacity constraints (smaller iwi lack technical expertise in co-governance negotiations); (c) financial asymmetry (settlements are always partial; Crown controls crown lands); (d) legal residue (Crown retains ultimate authority to legislate, override co-governance recommendations if it chooses). Theater ratio (0.64, contemporary): Moderate-high. Treaty settlement processes, co-governance structures, and Waitangi Tribunal protocols involve substantial ceremonial and performative elements — formal consultation rounds, written protocols, advisory committee structures — alongside genuine power-sharing. The theater reflects the scaffold phase: institutional structures that operationalize the partnership reading while managing the transition from snare to rope.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the historical snare (Māori powerless under unilateral Crown extraction) and the modern tangled rope (Māori organized but constrained, experiencing both partnership benefits and residual extraction). This gap reflects the judicial restoration of the partnership reading via contra proferentem doctrine. A secondary gap exists between the Crown's unilateral sovereignty framing (piton: ceremonial claim with no legal force) and the partnership reading (rope/tangled rope: binding legal framework). The analytical observer's risk is naturalizing the partnership reading as an immutable principle rather than recognizing it as a specific reading choice grounded in the Māori text.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by perspective and historical moment. For historical Māori interests (powerless/trapped/1840–1975): d ≈ 0.95 → f(d) ≈ 1.42 → experienced χ ≈ 0.92 × 1.42 × 1.0 = 1.30 (snare). For modern iwi collectives (organized/constrained/contemporary): d ≈ 0.50 → f(d) ≈ 0.65 → experienced χ ≈ 0.52 × 0.65 × 1.0 = 0.34 (tangled rope boundary, beneficiary-adjacent). For the partnership reading from the analytical position (analytical/analytical): d ≈ 0.72 → f(d) ≈ 1.15 → observed χ ≈ 0.52 × 1.15 × 1.0 = 0.60 (moderate). The directionality shifts because the structural relationship to the constraint (beneficiary vs victim, exit options) changes across perspectives and historical moments.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy in the traditional sense (choosing between type A and type B). Instead, it demonstrates that the constraint legitimately holds different types across different historical moments and perspectives: snare (1840–1975, powerless Māori), rope (partnership reading, organized iwi), tangled rope (contemporary hybridity, mixed coordination and extraction), scaffold (settlement apparatus with sunset logic), piton (Crown sovereignty doctrine as institutional inertia). The constraint's character migrated when the partnership reading was judicially restored. The question 'which type is correct?' is resolved by asking 'which reading of the Treaty controls?' — the Māori text, via contra proferentem. Once that reading is established as binding law, the tangled rope classification (contemporary, organized agent, constrained exit) is the correct frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maori_text_primacy_doctrine,
    'Does the Māori text of the Treaty control interpretation via contra proferentem (ambiguity resolved against the drafter), or do both texts stand as co-equal authority?',
    'Jurisprudential trend analysis: review Court of Appeal and Waitangi Tribunal decisions 1975–2026 for which text is cited as controlling, and whether contra proferentem is consistently applied when English and Māori texts diverge.',
    'If Māori text controls: the partnership reading (tino rangatiratanga retained) is the binding legal framework; Crown unilateral authority is foreclosed. If co-equal or English-controlling: the partnership reading coexists with the sovereignty-cession reading; ambiguity persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maori_text_primacy_doctrine, empirical, 'Whether Māori text controls via contra proferentem doctrine').

omega_variable(
    kawanatanga_semantic_boundary,
    'What is the precise structural boundary between kāwanatanga (governance, delegated authority requiring consent) and tino rangatiratanga (chieftainship, retained authority)? Does this boundary hold across different domains (land, resources, cultural institutions, law-making)?',
    'Comparative analysis of how courts and Tribunal apply the distinction in land rights, resource consent, co-governance, and iwi law-making cases. Identify which domains recognize tino rangatiratanga as autonomous vs. subject to kāwanatanga.',
    'If boundary is consistent and clear: partnership reading is structurally robust. If boundary is ambiguous or domain-variable: the reading is subject to competing interpretations and ongoing contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_semantic_boundary, conceptual, 'Structural boundary between kāwanatanga and tino rangatiratanga').

omega_variable(
    consent_mechanism_enforceability,
    'In contemporary co-governance and co-management arrangements (Ngāi Tahu resource consent, Tainui joint management, Tōpuni co-governance), is iwi/hapū consent actually binding on Crown decision-making, or is it advisory?',
    'Audit of Treaty settlement agreements and co-governance MOUs for consent definitions: binding veto, consent-with-override (Crown can act without consent if it overrides), or advisory consultation. Track actual decisions where Crown and iwi disagreed.',
    'If consent is binding: partnership reading is institutionally actualized. If advisory: the constraint remains tangled rope (mixed coordination and extraction) rather than pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_enforceability, empirical, 'Whether iwi/hapū consent in co-governance is binding or advisory').

omega_variable(
    translation_asymmetry_as_snare_mechanism,
    'To what extent did the English text''s ''sovereignty'' vs Māori text''s ''kāwanatanga'' difference function as a hidden extraction mechanism — allowing the Crown to exercise unilateral authority while claiming treaty fidelity?',
    'Historical-jurisprudential analysis: reconstruction of Crown intent in 1840 (did Crown negotiators know the texts diverged?), comparison with Māori oral traditions recorded post-1840, analysis of whether Crown''s 1840–1975 actions align with the partnership reading or the sovereignty-cession reading.',
    'If intentional ambiguity: the snare perspective is validated — the constraint was designed to conceal extraction. This moves responsibility from mere misinterpretation to structural deception. If unintentional: the snare was an emergent artifact of translation practices, not a deliberate mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_as_snare_mechanism, empirical, 'Whether translation asymmetry was intentional or emergent snare mechanism').

omega_variable(
    kernel_reading_foreclosure_moment,
    'At what historical moment(s) was the partnership reading foreclosed by Crown practice, and at what moment(s) was it restored by judicial recognition?',
    'Timeline of Crown policy shifts and key judicial decisions: identify the decision-point when the Crown moved from treating the Treaty as binding partnership (if ever) to treating it as historical artifact with no current force; identify the judicial turning point (Waitangi Tribunal Act 1975, Re Ngāi Tahu Fisheries, Ngāi Tahu and Ngāti Whare cases) when the partnership reading re-entered law as binding.',
    'Understanding the foreclosure and restoration moments clarifies whether the constraint was always tangled rope (partnership + extraction), or whether it migrated from rope (partnership) to snare (foreclosed) to tangled rope (judicially restored partnership with residual extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_moment, empirical, 'Historical moments of partnership reading foreclosure and restoration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 186).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_theater_1840_founding, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(treaty_theater_1890_land_wars, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(treaty_theater_1940_apartheid, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(treaty_theater_1975_tribunal_act, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 135, 0.58).
narrative_ontology:measurement(treaty_theater_2020_contemporary, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 180, 0.64).

% Extraction over time
narrative_ontology:measurement(treaty_extract_1840_founding, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(treaty_extract_1890_land_wars, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(treaty_extract_1940_apartheid, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(treaty_extract_1975_tribunal_act, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 135, 0.42).
narrative_ontology:measurement(treaty_extract_2020_contemporary, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 180, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(treaty_suppress_1840_founding, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(treaty_suppress_1890_land_wars, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(treaty_suppress_1940_apartheid, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(treaty_suppress_1975_tribunal_act, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 135, 0.55).
narrative_ontology:measurement(treaty_suppress_2020_contemporary, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 180, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_self_determination_legal_capacity).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, land_alienation_purchasing_regime).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, iwi_representation_in_governance).

% DUAL FORMULATION NOTE:
% The rangatiratanga_retention_reading is paired with crown_cession_reading and biculturalism_reading as three distinct constraint stories emerging from the same treaty kernel. Each reading has different structural consequences: the rangatiratanga reading enables co-governance constraints; the crown_cession reading removes Māori veto power (forecloses partnership); the biculturalism reading positions both as complementary and potentially in tension. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
