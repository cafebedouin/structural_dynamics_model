% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Legitimacy Regime for Territorial Sovereignty
 *   domain: political theory / international relations
 *
 * SUMMARY:
 *   This story instantiates the covenant_continuity_reading of the
 *   territorial_sovereignty_legitimacy kernel: the claim that sovereignty
 *   over the land rests on an ancient covenantal grant, sustained by
 *   continuous Jewish presence through the exile centuries, and ratified by
 *   modern international instruments (the Balfour Declaration, UN Partition
 *   Plan Resolution 181, and the 1948 establishment). The standing
 *   arrangement under contest is the operative sovereignty regime that this
 *   doctrine legitimates: state institutions administering the territory,
 *   settlement growth administered as return, and an enforcement apparatus
 *   defending claim exclusivity. The reading's own structural signature
 *   extends temporal scope to the biblical period, holds the legitimacy claim
 *   intact through demographic absence, reads partition as compromise of a
 *   pre-existing right rather than creation of a new one, and frames
 *   settlement as return rather than colonization. This file is one member of
 *   a three-story kernel family; the epsilon referent is the standing
 *   arrangement, and the epsilon value is indexed to THIS reading's
 *   assessment of it, not to the sibling readings' assessments and not to the
 *   arrangement this reading would prefer.
 *
 * KEY AGENTS:
 *   - - israeli_state_institutions: agenda-setter (institutional/arbitrage) — administers the sovereignty regime, controls settlement policy and citizenship boundaries, collects legitimacy rents, and simultaneously bears defense, enforcement, and diplomatic-isolation costs
 *   - - religious_zionist_settlement_movement: primary beneficiary (organized/identity_locked) — receives land, subsidies, and existential meaning; its self-concept is constituted through the covenant-return relationship
 *   - - diaspora_jewish_advocacy_networks: beneficiary (powerful/constrained) — collects identity anchoring and an advocacy vehicle at geographic remove, carrying little direct cost of the arrangement
 *   - - palestinian_arab_residents: primary target (organized/trapped) — organized politically but governed by a regime adjudicating title against their claims; exit blocked by borders, statelessness risk, and blockade
 *   - - descendants_of_1948_refugees: target and excluded voice (powerless/trapped) — bear the deepest costs and are locked out of the framework adjudicating return
 *   - - neighboring_host_state_communities: secondary cost-bearer (moderate/constrained) — absorb multi-generational refugee maintenance with limited recourse
 *   - - international_recognition_bodies: inter-institutional actor (institutional/analytical) — extend or withhold the ratification the doctrine cites; their instruments are load-bearing for the legitimacy claim
 *   - - international_legal_scholars: analytical observer — sees the full structure including the tension between the doctrine's internal coherence and its external legal reception
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.48).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Legitimacy Regime for Territorial Sovereignty").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political theory / international relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_advocacy_networks).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, descendants_of_1948_refugees).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, neighboring_host_state_communities).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, ancient_covenant_title_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_presence_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, balfour_declaration).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, un_partition_plan_resolution_181).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers sovereignty policy: citizenship boundaries, settlement authorization, land registration, and the diplomatic defense of the legitimacy doctrine in international fora. Collects territorial control, taxation authority, conscription capacity, and international standing. Simultaneously pays for the regime's upkeep: defense budgets, enforcement deployments, casualty exposure, and the diplomatic costs of contested recognition. Its flexibility lies in shifting among justification registers (scriptural, historical, legal, security) as audiences require.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, beneficiary).

% Populates and builds the settlement enterprise under the return framing, receiving land allocations, infrastructure subsidies, and educational support. Draws existential meaning from inhabiting land understood as covenantally promised; leaving would not merely forfeit subsidies but dissolve a constitutive self-understanding. Supplies much of the ideological energy and personnel for the claim-maintenance apparatus.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Resides outside the territory while drawing identity anchoring, philanthropic channels, and a political-advocacy vehicle from the arrangement's persistence. Bears few direct operational costs; exposure is limited to reputational and backlash effects. Public disavowal of the legitimacy doctrine carries heavy communal-social price, which bounds individual exit even where geography permits it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_advocacy_networks, beneficiary,
    powerful, generational, constrained, global).

% Live under a sovereignty regime whose founding doctrine adjudicates title against their claims, maintaining their own political parties, municipal councils, and civic organizations within it. Experience movement restrictions, permit regimes, land expropriations, and subordinate legal-administrative status. Borders, blockade, and statelessness risk close emigration as a practical path, while full participation in the title-adjudicating framework is unavailable.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents, payer,
    organized, generational, trapped, regional).

% Inherit the displacement of 1948 across camp and diaspora communities in multiple countries. Hold no vote in any framework that adjudicates return to the homes their families left, while the governing doctrine holds that demographic absence did not extinguish the title of the returning population — an asymmetry they experience as the sharpest edge of the arrangement. Dispersal across host countries fragments any unified political voice.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, descendants_of_1948_refugees, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, descendants_of_1948_refugees, excluded).

% Communities in Jordan, Lebanon, Syria, and Egypt that have absorbed and maintained refugee populations across generations, bearing fiscal and social costs with limited international compensation and no ability to relocate the burden. Their states' treaty relationships with the sovereign regime constrain how loudly they press the issue.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, neighboring_host_state_communities, payer,
    moderate, generational, constrained, regional).

% States, the United Nations, and allied multilateral bodies whose instruments — the Balfour Declaration, Resolution 181, recognition decisions — are cited within the legitimacy doctrine as its ratifying layer. They periodically reaffirm, qualify, or withdraw elements of that ratification through resolutions, advisory opinions, and bilateral statements, making their output load-bearing for the doctrine they did not design.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, observer,
    institutional, generational, analytical, global).

% Academic lawyers and theorists who analyze the doctrine's fit with treaty law, occupation law, and self-determination norms. They take no side in the underlying conflict and bear none of its costs, but their publications shape the interpretive environment in which the recognition bodies operate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_legal_scholars, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a territorially dispersed, historically persecuted minority into a single national project with one legitimacy narrative, solving collective-action problems in immigration absorption, institution-building, diaspora mobilization, and international alignment that fragmented communal structures could not solve.
% TRANSFER_FUNCTION: Moves territorial control, residency rights, settlement land, and international standing toward the Jewish national project; moves displacement, restricted movement, and subordinate claim-status onto the Palestinian Arab population, with long-run refugee-maintenance costs pushed onto neighboring host communities.
% ABSENT_VOICES: Refugee-descendant communities are the sharpest absence: the framework adjudicates title over the homes they lost while they hold no vote in it. Descendants of pre-Mandate residents were not consulted when the recognition instruments were drafted. Anti-Zionist ultra-Orthodox communities rejecting the covenant-political synthesis and Palestinian citizens opposing the doctrine's exclusivity are present in the polity but structurally outvoted rather than absent.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy regime vanished overnight, the state's claim architecture would lose its ratification layer and its settlement authorization basis simultaneously: settlement policy would face immediate legal vacuum, diaspora mobilization structures would fragment, neighboring states and recognition bodies would reopen every border and title question settled under the doctrine, and the identity economies built on the return narrative would collapse. Virtually no arrangement in the region depends on more parties' conduct than this one.
% FOUNDING_PROBLEM: Securing permanent refuge and collective safety for a stateless, repeatedly persecuted minority by converting an inherited covenantal title into operative sovereignty with international warrant.
% FOUNDING_PROBLEM_CORROBORATION: The state and movement attest the problem is live, citing documented mass-casualty attacks and multi-front warfare as evidence that the safety dimension persists. From outside the benefiting parties: mid-century diplomatic archives and immigration records show the statelessness-refuge dimension was declared substantially resolved within a generation of statehood as survivor absorption proceeded, while human-rights monitoring bodies and international court records characterize the arrangement's current operation as maintenance and expansion of acquired position — together attesting that the founding problem has been transformed, not simply that it persists or died.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is tangled_rope and the metrics are authored independently of it. Coordination function is real: the doctrine solved a genuine collective-action problem for a dispersed, persecuted minority — one legitimacy narrative enabled immigration coordination, institution-building, diaspora mobilization, and international alignment that fragmented communal structures could not deliver. Asymmetric extraction rides the same structure: the population governed without consent, the refugee-descendant population denied return, and host communities absorbing displacement costs all pay through the arrangement's operation, which requires continuous active enforcement (military administration, checkpoints, settlement administration, diplomatic defense) to hold. Extractiveness (0.48) is reading-indexed: this reading attributes the governed population's costs to conflict conditions and security necessity rather than to the doctrine itself, holding the arrangement substantially rightful, yet it cannot deny that millions live under administration they did not consent to — hence a moderate value well below what a critical sibling reading would author over the identical referent. Suppression (0.68) is a raw structural property, unscaled by power or scope: the enforcement machinery that keeps alternative claim-frameworks from operating is extensive and hardened, and the trajectory shows enforcement-capacity buildup (1948, 1967), partial relaxation at Oslo (recognition exchange reduced enforcement need), then ratcheting again through barrier construction and administrative entrenchment. Accessibility_collapse is low (0.32) because alternative legitimacy frameworks have not collapsed at all — the sibling readings of this very kernel are live and vigorous, which is precisely the observable that separates this construct from a natural-law claim. Resistance is high (0.78): uprisings, boycott movements, universal-jurisdiction litigation, advisory proceedings, and recurring General Assembly majorities all contest the arrangement. Theater_ratio (0.34) reflects a growing performative layer — archaeological projects marshaled as title evidence, anniversary and ceremony politics, symbolic legislation — layered over still-substantive governing function.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply divergent types across seats. From the agenda-setter seat (state institutions), the arrangement presents as the successful solution to an existential coordination problem it built and maintains — coordination-forward, with enforcement read as self-defense. From the trapped payer seats, the identical structure operates as enforced subordination of their claims: the doctrine's temporal extension to antiquity is experienced as a moving wall that no modern demographic fact can penetrate. Identity-locked beneficiaries (settlement movement) experience the arrangement as covenant fulfillment — a subsidy in the fullest sense, including existential meaning — while the excluded refugee-descendant seat experiences the same doctrine as the instrument that froze their dispossession. The diaspora seat computes near the pure-beneficiary pole: legitimacy goods flow outward to it while its cost-bearing is indirect. Same-power lateral contrast: Palestinian municipal councils and settlement regional councils hold formally analogous local-government standing, but their exit options differ categorically — one set is mobile within a state system, the other trapped behind a permit regime — so nominal equal standing produces opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive derivation: state institutions and settlement movement derive near the beneficiary pole; diaspora networks, as beneficiaries with constrained-but-real disengagement options, sit close behind them; the trapped, powerless refugee-descendant seat derives nearest the full-target pole; resident and host-community payers sit high. One override is declared: israeli_state_institutions at power atom institutional receives d = 0.30, because a pure beneficiary derivation undershoots the state's actual net position — it collects legitimacy rents but also pays the enforcement budget, conscription burden, casualty exposure, and diplomatic-isolation costs, making it a genuine dual-positioned agent (agenda_setter with secondary_role beneficiary) rather than a free rider. Note the override keys on power atom per schema, so it applies at that granularity; the analytical-exit observer seats sharing the atom are handled by their own exit class. The identity_locked exit of the settlement movement pins it nearer the full-beneficiary end than its organizational power alone would suggest: fusion with the covenant-return narrative removes exit as a live option regardless of cost shifts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing permanent refuge and collective safety for a stateless, repeatedly persecuted minority — has an ambiguous status: the statelessness-refuge dimension was substantially addressed within a generation of statehood, while the safety dimension remains demonstrably live (documented mass-casualty attacks and multi-front war). Authoring founding_problem_status as contested, with corroboration drawn from outside the benefiting parties (diplomatic archives, human-rights monitoring, international court records), prevents the two symmetrical mandatrophy failures: reading the arrangement as a dead-mandate shell persisting by inertia (wrong — the coordination and defense functions remain substantive, theater_ratio well below piton range), and reading it as a fully live original mission (also wrong — a substantial share of current enforcement defends acquired position beyond any refugee-security function the founders specified). The tangled_rope claim is what preserves both truths simultaneously; resolving the omega variables on the presence-asymmetry principle and the recognition-instrument status will determine whether the extraction component stabilizes or deepens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the covenant_continuity_reading of the territorial_sovereignty_legitimacy kernel; how much of its measured structure is indexical to that reading rather than to the underlying situation?',
    'Cross-reading comparison within the kernel family: the self_determination_reading flips the beneficiary/victim demographic mapping and dates the relevant presence window to the modern era; the existential_matrix_reading dissolves the juridical frame entirely and predicts identical extraction under any legal regime.',
    'Classification is per-reading by design. Divergence across the three sibling stories measures the location and depth of the kernel dispute itself; it must never be averaged back into a single constraint or treated as measurement error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of a contested sovereignty-legitimacy kernel.').

omega_variable(
    covenant_title_naturalness,
    'Is the covenant-title premise, for those who hold it, a fixed feature of reality (divinely granted, therefore beyond construction) or a socially maintained claim whose persistence depends on institutional enforcement and identity investment?',
    'Irreducible at the theological level; behavioral proxy available: observe whether claim-maintenance intensity tracks enforcement costs and subsidy flows, or persists in constituencies bearing pure cost with no concentrated return.',
    'If the title claim operates as constructed-and-enforced despite being experienced by adherents as natural, the arrangement''s coordination component is smaller than its beneficiaries report and the extraction component correspondingly larger; if genuinely experienced as fixed, the identity_coordination floor does more explanatory work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_title_naturalness, conceptual, 'Natural-law versus constructed-character ambiguity of the covenant-title premise.').

omega_variable(
    demographic_absence_asymmetry_principle,
    'The doctrine treats Jewish demographic absence during the exile centuries as non-extinguishing of title while treating Palestinian displacement from 1948 as extinguishing theirs. Is this asymmetry principled (covenantal inalienability; voluntary versus coerced departure) or ad hoc favoritism serving the benefiting parties?',
    'Comparative doctrinal analysis of the continuity principle as applied elsewhere, combined with historiographic reconstruction of 1948 departure causes (expulsion orders versus flight versus panic) at village level.',
    'A principled distinction strengthens the coordination reading of the doctrine; an ad hoc one deepens the asymmetric-extraction component and pushes computed classifications toward the snare end for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_asymmetry_principle, empirical, 'Whether the presence/asymmetry rule is principled or self-serving.').

omega_variable(
    suppression_structural_vs_internalized,
    'For identity-fused constituencies (settlement movement, parts of the diaspora), is continued claim-maintenance driven by structural enforcement (land subsidies, legal privileges, state ceremonial reinforcement) or by internalized identity fusion that would persist if the structural supports were removed?',
    'Post-exit trajectory analysis of constituencies that have disengaged (emigrants, ideologically departed communities): if covenant-commitment decays quickly after exit from the supporting institutions, suppression was structural; if it persists, it is internalized.',
    'Internalized fusion raises effective suppression above the structural measure and makes the arrangement robust to policy-level reform; purely structural dependence makes it sensitive to funding and legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression mechanism between structural enforcement and internalized identity.').

omega_variable(
    recognition_instrument_status,
    'Do the Balfour Declaration and UN Partition Plan confirm a pre-existing right (this reading''s claim) or create a new one (the self_determination_reading''s implication)? The entire transfer baseline shifts on the answer.',
    'Located disagreement: examine drafting records and the instruments'' own language (Balfour''s caveat clause; Resolution 181''s reciprocal state structure) for whether they presuppose antecedent title or allocate fresh rights.',
    'Confirmation-framing keeps Palestinian claims in the position of negotiated compromise; creation-framing makes the 1947 allocation the origin point and the subsequent expansion a departure from the created settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_instrument_status, conceptual, 'Where this reading and the self-determination sibling locate the origin of title in the recognition instruments.').

omega_variable(
    settlement_character_framing,
    'Are post-1967 settlements acts of return under a continuing pre-existing title, or colonization under a new occupation regime? This is the second located disagreement with the sibling readings.',
    'Legal analysis under the Geneva Convention framework versus internal doctrinal analysis under the covenant frame; the two bodies of law answer differently by construction, so the omega resolves only by choosing which body governs.',
    'Return-framing places settlements inside the legitimate sovereignty arrangement; colonization-framing places them outside it as a separate extractive layer, materially raising the arrangement''s measured cost profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_character_framing, conceptual, 'Normative character of settlements under this reading versus sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement_basis(terr_tr_t1917, observed).
narrative_ontology:measurement(terr_tr_t1922, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement_basis(terr_tr_t1922, observed).
narrative_ontology:measurement(terr_tr_t1939, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1939, 0.12).
narrative_ontology:measurement_basis(terr_tr_t1939, observed).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.24).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement_basis(terr_tr_t2005, observed).
narrative_ontology:measurement(terr_tr_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2025, 0.34).
narrative_ontology:measurement_basis(terr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.14).
narrative_ontology:measurement_basis(terr_be_t1917, observed).
narrative_ontology:measurement(terr_be_t1922, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1922, 0.17).
narrative_ontology:measurement_basis(terr_be_t1922, observed).
narrative_ontology:measurement(terr_be_t1939, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1939, 0.21).
narrative_ontology:measurement_basis(terr_be_t1939, observed).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.34).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.44).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.39).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement_basis(terr_be_t2005, observed).
narrative_ontology:measurement(terr_be_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(terr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.1).
narrative_ontology:measurement_basis(terr_su_t1917, observed).
narrative_ontology:measurement(terr_su_t1922, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1922, 0.12).
narrative_ontology:measurement_basis(terr_su_t1922, observed).
narrative_ontology:measurement(terr_su_t1939, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1939, 0.18).
narrative_ontology:measurement_basis(terr_su_t1939, observed).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.51).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement_basis(terr_su_t2005, observed).
narrative_ontology:measurement(terr_su_t2025, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement_basis(terr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Israel's legitimacy' decomposes into three structurally distinct constraint stories under the epsilon-invariance principle, because the label conflates three incompatible accounts of where title originates (covenant-plus-ratification; modern demographic consent; existential necessity prior to law). Each story carries its own stable epsilon, beneficiary/victim mapping, and classification over the same standing referent; this upstream story (highest doctrinal elaboration and longest transmission chain) exerts structural pressure on both siblings — its pre-existing-right framing resets what the self-determination sibling must argue against, and its persistence as the operative legal frame pressures the existential sibling's claim that law is irrelevant. Cross-family contamination flows through the recognition-instruments node: degradation of the ratification layer (advisory rulings, recognition withdrawals) propagates to this reading first, then through it to the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
