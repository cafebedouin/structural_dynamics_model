% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Reading: Pharaonic Monopoly on Divine Legitimacy
 *   domain: religious/political
 *
 * SUMMARY:
 *   Akhenaten's Atenist reading of divine legitimacy claims that Aten is the
 *   exclusive true deity, that all other gods are false, and that the pharaoh
 *   alone receives Aten's revelation. This reading centralizes interpretive
 *   authority in the pharaonic office, dismantles the traditional Amun
 *   priesthood's temple economies and doctrinal authority, suppresses folk
 *   syncretic practice, and reorganizes Egypt's religious and political
 *   economy around pharaonic monopoly. The constraint is authored as Tangled
 *   Rope: it coordinates a unified state religion (genuine coordination
 *   function) while simultaneously extracting from established priesthoods,
 *   folk practitioners, and local temple economies (asymmetric extraction via
 *   suppression). The claim/metric independence principle applies: this
 *   reading is CLAIMED as Tangled Rope based on its structural simultaneity
 *   of coordination and extraction; the metrics describe substantially high
 *   extractiveness, intense suppression, and rising theater (the functional
 *   coordination rationale declining as enforcement hardens). The engine will
 *   compute each seat's independent type from the structural data—the
 *   pharaonic seat may compute as Snare, the priesthood seat as Snare or
 *   Tangled Rope, the folk practitioners as Snare. The authored claim does
 *   not reconcile these per-seat divergences; divergence is the measurement.
 *
 * KEY AGENTS:
 *   - Pharaonic Authority: Monopolizes interpretive access to Aten; centralizes temple wealth; enforces the reading via administrative suppression.
 *   - Established Priesthoods (Amun temples): Bear material loss and doctrinal delegitimization; their alternative reading is actively suppressed; trapped by institutional identity and land holdings.
 *   - Folk Practitioners: Forced to abandon household ritual; identity-locked by community role and protective belief systems; internalized suppression.
 *   - Aten Priesthood: Newly empowered, benefits from centralized authority and redistributed temple wealth; exits available but align with pharaonic claim.
 *   - Competing Religious Authorities: Excluded from public legitimacy; pushed underground; their alternative cosmologies are the primary suppression target.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.89).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading: Pharaonic Monopoly on Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '1917615c-2114-46f1-a89e-e9d53a49d1b6').
narrative_ontology:cs_kernel_codification('1917615c-2114-46f1-a89e-e9d53a49d1b6', formalized).
narrative_ontology:cs_authority_grounding('1917615c-2114-46f1-a89e-e9d53a49d1b6', extraction).
narrative_ontology:cs_interpretation_layer_present('1917615c-2114-46f1-a89e-e9d53a49d1b6').
narrative_ontology:cs_reading_relation('1917615c-2114-46f1-a89e-e9d53a49d1b6', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('1917615c-2114-46f1-a89e-e9d53a49d1b6', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('1917615c-2114-46f1-a89e-e9d53a49d1b6', foundational, aten_cosmological_exclusivity).
narrative_ontology:cs_axiom_status(aten_cosmological_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('1917615c-2114-46f1-a89e-e9d53a49d1b6', aten_cosmological_exclusivity, empirically_contingent).
narrative_ontology:cs_axiom('1917615c-2114-46f1-a89e-e9d53a49d1b6', foundational, pharaoh_sole_divine_intermediary).
narrative_ontology:cs_axiom_status(pharaoh_sole_divine_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('1917615c-2114-46f1-a89e-e9d53a49d1b6', pharaoh_sole_divine_intermediary, deontological).
narrative_ontology:cs_reference_frame('1917615c-2114-46f1-a89e-e9d53a49d1b6', divine_revelation_pharaonic_monopoly).
narrative_ontology:cs_drift_state('1917615c-2114-46f1-a89e-e9d53a49d1b6', post_pharaonic_death, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1917615c-2114-46f1-a89e-e9d53a49d1b6', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, established_priesthoods).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, local_temple_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_administrative_apparatus).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive revelatory access to Aten's truth. Sets the religious orthodoxy by decree; interprets all doctrine; monopolizes the temples and their revenues. Enforces the claim through administrative authority, control of state resources, and suppression of competing religious practice. Justifies the arrangement as spiritual truth and unified national coherence under divine guidance. Benefits directly from eliminating rival power centers among the priesthoods.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Formerly held interpretive authority over divine cosmology and managed large temple economies with land, servants, and wealth. Under the reading, their doctrines are declared false; their institutional power is dismantled; their temple economies are absorbed or redirected to Aten worship under pharaonic control. They bear both material loss and the loss of interpretive legitimacy. Their alternative reading—that Amun-Ra and the traditional pantheon are the true cosmological reality—is actively suppressed. They could theoretically exit by fleeing the delta, but institutional identity and land holdings trap most within the system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, established_priesthoods, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, established_priesthoods, excluded).

% Conduct household and village ritual incorporating local gods, protective spirits, and syncretic household cults alongside state-mandated Aten worship. Under the reading, their practices are declared spiritually false and forbidden; enforcement includes administrative pressure, temple closure, and social stigma. Their exit options are severely constrained: they cannot leave the villages (economic dependency), and their ritual identity is fused with their community role and household protection beliefs. They carry the suppression internally as internalized shame and conflict between household tradition and state mandate.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Regional temple centers that provided social services, grain storage, irrigation coordination, and local employment. When their patron deities are declared false and their revenues redirected to Aten temples, the economic functions they provided are either eliminated or absorbed into pharaonic administration. Local communities bear the loss of these services and the disruption of economic stability.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, local_temple_economies, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_non_agent(divine_legitimacy_substrate__atenist_monotheistic_reading, local_temple_economies).

% The scribal and military bureaucracy that enforces the religious monopoly. Gains power and resources through centralization of temple wealth and elimination of competing power centers. Administers the suppression machinery and benefits from the enhanced pharaonic authority the religious monopoly produces.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_administrative_apparatus, beneficiary,
    institutional, generational, arbitrage, continental).

% A newly created or promoted priesthood claiming exclusive interpretive authority over Aten doctrine under pharaonic guidance. Receives temple lands, resources, and social authority that were formerly distributed among the traditional priesthoods. Their doctrinal readings are vindicated by state power; their material interests align with the pharaoh's claim to monopoly. Exit is available: they could theoretically reject the reading, but their status and wealth depend on its maintenance.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood, beneficiary,
    powerful, generational, mobile, continental).

% Other priesthoods and spiritual authorities who hold the polytheistic or syncretic readings. Are explicitly barred from public practice, temple access, and official legitimacy. They are not eliminated outright but are pushed underground or into private household contexts. Their exclusion is the primary enforcement object—the suppression machinery exists to maintain their marginalization and the pharaoh's interpretive monopoly.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, competing_religious_authorities, excluded,
    powerful, generational, trapped, continental).

% Foreign powers and trade networks (Hittites, Mesopotamian cities, etc.) that observe the Egyptian religious restructuring. They interact with Egypt's diplomatic and economic relations but are not direct parties to the internal religious constraint. Some adopt elements of Aten theology in diplomatic context; others maintain skepticism. They provide external evidence about how the reading is perceived beyond Egypt.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, external_trading_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_authority).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to unify Egyptian religious practice under a single cosmological framework and a single interpretive authority (the pharaoh), eliminating factional conflict among competing priesthoods and creating ideological coherence across the kingdom.
% TRANSFER_FUNCTION: Transfers temple wealth, interpretive authority, and spiritual legitimacy from the established priesthoods and folk practitioners to the pharaonic authority and the newly empowered Aten priesthood; folk practitioners lose ritual autonomy and must adopt state-mandated worship; established priesthoods lose both material resources and doctrinal authority.
% ABSENT_VOICES: The polytheistic priesthoods and folk practitioners are not absent but actively excluded: their alternative readings of the divine legitimacy kernel are suppressed, not debated. A competing priesthood would argue that Amun-Ra and the traditional pantheon remain the true cosmic order; folk practitioners would argue that household gods and syncretistic practice remain spiritually valid. These voices are pushed underground or silenced by enforcement. The historical record does not preserve their articulated counter-readings because articulation itself is forbidden under the constraint.
% DISAPPEARANCE_RATIONALE: If the Atenist monopoly constraint vanished overnight—if the pharaonic claim to exclusive revelatory authority and the suppression apparatus that enforces it collapsed—competing priesthoods would re-emerge, temple economies would reorganize around their restored patron deities, folk practitioners would resume public ritual without shame, and the redistributed temple wealth would flow back to dispersed institutional authority. The religious and economic landscape would restructure dramatically; the unity the constraint imposed would dissolve.
% FOUNDING_PROBLEM: Religious factional conflict among competing priesthoods threatened national coherence; divergent household practices and village cults created cosmological confusion; Amun-Ra priesthood monopolies in certain regions challenged pharaonic centralization of power.
% FOUNDING_PROBLEM_CORROBORATION: The pharaonic authority and the newly empowered Aten priesthood attest the founding problem is live and that the reading solves it. Competing priesthoods and historical analysts outside the benefiting parties attest that the 'factional conflict' was a normal feature of Egyptian religious life and that centralization served primarily to concentrate pharaonic power and wealth, not to solve genuine dysfunction—that the problem narrative is post-hoc justification rather than original cause.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.68 to 0.82 over the interval as the pharaonic authority consolidates control: early implementation faces institutional resistance and incomplete enforcement; by year 16–20, the new arrangement stabilizes at near-maximum extraction as competing priesthoods are subordinated and their economic bases absorbed. Suppression requirement rises steeply from 0.72 to 0.89 because maintenance of the monopoly demands continuous enforcement—the alternative readings (polytheistic, syncretic) are not naturally foreclosed; they must be actively kept underground. Theater rises from 0.25 to 0.41 because initial years emphasize genuine religious transformation and coordination benefits; later, an increasing share of enforcement activity is devoted to suppressing private worship and maintaining doctrinal conformity rather than providing the coordination function. The measurement grid is aligned across all three metrics; every time point represents an authored assessment of all three at that moment. The basis field distinguishes early projections (years 0–1, before full implementation) from observed historical records (years 3+).
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic seat and the Aten priesthood perceive coordination benefits and spiritual truth; they experience the reading as establishing necessary unity and cosmological clarity. The established priesthood seat experiences loss of institutional power, economic devastation, and delegitimization; they experience the reading as enforced extraction disguised as religious reform. Folk practitioners experience loss of ritual autonomy, internal conflict between household tradition and state mandate, and identity-level suppression; they experience the reading as spiritual oppression. The engine's per-seat classification computes this divergence from the structural data: pharaonic and Aten priesthood seats may compute as Rope or Tangled Rope (coordination, low extraction from their position); established priesthood and folk practitioner seats compute as Snare (high extraction, suppression, forced participation). The authored claim does not flatten these differences; it names them.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaonic authority and Aten priesthood: directionality near 0.0–0.3 (beneficiary end). They collect from the arrangement via consolidated temple wealth, interpretive monopoly, and eliminated competition. Their exit options are arbitrage-grade (they could theoretically change the reading, but it aligns with their power and wealth, so they stay). Established priesthoods: directionality near 0.8–1.0 (target end). They pay through material loss and doctrinal suppression; their exit options are constrained (land holdings and institutional identity trap them). Folk practitioners: directionality near 0.9–1.0 (full target end). They pay through forced conversion, lost autonomy, and identity-level suppression; their exit options are identity-locked (they cannot leave without abandoning community and household role). Competing religious authorities: structurally excluded, directionality undefined in the framework—they are not parties to the constraint's normal operation but to its maintenance via suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—factional conflict among priesthoods—is contested. The pharaonic narrative claims it was a live dysfunction; competing sources attest it was a normal institutional feature and that the reading's true function was centralization of power and wealth. The mandatrophy question is whether the constraint persists because its founding problem remains live or because the constraint itself has become self-maintaining through institutional inertia. The measurement series showing rising extraction and theater suggests drift toward self-maintenance: early years show genuine coordination (lower extraction, lower theater) as the religious system unifies; later years show declining coordination rationale (rising theater as pure suppression) coupled with rising extraction (concentration of wealth proceeds independently of the coordination function). This drift pattern is consistent with mandatrophy—the constraint persists past the point where the founding problem would justify it, maintained now by the institutional interests it created. The theater rise from 0.25 to 0.41 is diagnostic: if theater had remained low, the coordination function would still be primary; instead, theater rises as enforcement hardens, indicating Goodhart drift (the coordination measure is increasingly displaced by the extraction maintenance goal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmological_truth_vs_power_consolidation,
    'Is the Atenist reading grounded in genuine cosmological discovery/revelation about the nature of the divine, or does it function primarily as a vehicle for pharaonic power consolidation?',
    'Historical comparison with other monotheistic movements and their institutional effects; analysis of whether the timing of the revelation coincides with pharaonic centralization needs rather than with cosmological/theological developments; study of whether the reading persists if divorced from pharaonic authority.',
    'If grounded in cosmological truth, the constraint is a legitimate coordination response to spiritual error; if primarily a power consolidation vehicle, the extraction and suppression components become the primary function and the coordination narrative becomes post-hoc cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmological_truth_vs_power_consolidation, conceptual, 'Whether the monotheistic claim is independent of its power-centralizing function or dependent on it.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of folk practice and competing priesthoods primarily maintained through structural barriers (administrative enforcement, temple closure, resource control) or through internalized belief in the reading''s truth?',
    'Post-pharaonic historical observation: if folk practitioners and underground priesthoods abandon the suppressed readings when enforcement lapses, suppression was primarily structural; if the readings persist as internalized shame or residual belief even after enforcement is removed, suppression became partially internalized.',
    'Structural suppression is reversible; internalized suppression is path-dependent and may persist even if the external constraint structure collapses. If suppression is internalized, the reading''s effective suppression is higher than the administrative measure suggests, and the constraint''s hold is more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is enforced externally or has been internalized by the suppressed parties.').

omega_variable(
    reading_identity_boundary,
    'Is this reading a single coherent position (Aten as exclusive true deity, pharaoh as sole legitimate interpreter) or does it decompose into multiple structurally distinct constraints when examined at finer grain?',
    'Examine whether rejecting the pharaonic interpretive monopoly would logically entail rejecting Aten''s cosmic exclusivity, or vice versa. If the two could be held separately—a different priesthood might also claim Aten is the true deity but dispute pharaonic monopoly—then the reading is actually two constraints and should be decomposed per the ε-invariance principle.',
    'If the reading decomposes, one story should be generated for the monotheistic cosmological claim and a separate story for the pharaonic interpretive monopoly. They would have different ε values (the cosmological claim might be low-extraction if widely accepted; the monopoly claim is high-extraction due to institutional power centralization). The current story would become one branch of a constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_boundary, conceptual, 'Structural coherence of the monotheistic claim versus the pharaonic monopoly claim.').

omega_variable(
    kernel_read_by_committer,
    'Is this constraint a genuine independent reading of the divine-legitimacy kernel, or is it a collapse of multiple kernel framings because the Akhenaten evidence is sparse and ambiguous?',
    'Examine historical texts (Amarna letters, temple reliefs, biographical material) to assess whether they support a single coherent Atenist reading or whether later interpreters have synthesized a reading that was never explicitly articulated in one framework during Akhenaten''s reign.',
    'If later synthesized, the constraint''s kernel_context may misrepresent the historical structure of the dispute. The actual kernel might be fragmented or implicit rather than three coherent competing readings. This is a metadata-level omega about the adequacy of the committer framing itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_read_by_committer, conceptual, 'Whether the atenist_monotheistic_reading is a coherent historical reading or a modern historiographical construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(divi_tr_t7, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 7, 0.33).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(divi_tr_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(divi_be_t7, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 7, 0.76).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(divi_be_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 16, 0.82).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(divi_su_t7, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 7, 0.84).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement(divi_su_t16, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 16, 0.89).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 20, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.18).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three constraint stories, one per reading of how divine legitimacy flows in Egypt. This story (atenist_monotheistic_reading) represents the pharaonic claim to exclusive revelatory access and cosmological truth; it structurally forecloses the polytheistic reading but coexists_with (while suppressing) the syncretic reading. All three readings are linked via network.affects_constraints. The three stories share a common referent (the question of divine legitimacy structure in Egypt) but differ in ε (the pharaonic reading is highly extractive; the polytheistic reading treats traditional distribution as coordinate; the syncretic reading focuses on household pragmatism rather than institutional authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
