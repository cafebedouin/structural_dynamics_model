% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Scaffolding Gate on Zero-as-Number Operational Entry
 *   domain: history of mathematics/philosophy of mathematics/conceptual history
 *
 * SUMMARY:
 *   Zero-as-number was mathematically available — latent in the logic of
 *   positional notation — but became operationally thinkable only inside a
 *   compatible conceptual scaffold. Indian philosophical traditions supplied
 *   that scaffold first; Islamic codification standardized and transmitted
 *   it; European traditions, locked into Greek-derived geometric algebra and
 *   Aristotelian hostility to nothingness, crossed the gate roughly seven
 *   centuries later and paid heavily en route. On this reading, contact did
 *   not ship a concept so much as trigger recognition of a structure the
 *   recipients already possessed the raw materials for. This file
 *   instantiates the hybrid_scaffolding_reading of kernel
 *   zero_as_number_entry. The colloquial label 'the discovery of zero'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct claims: the contingent sibling authors epsilon for a
 *   metaphysically walled-off concept, the universal sibling authors epsilon
 *   for an always-available structure, and this reading authors epsilon for a
 *   latent-but-gated structure — moderate on both the necessity side and the
 *   contingency side. Each sibling is a separate file with its own epsilon,
 *   beneficiary/victim structure, and classification; this file links them
 *   through network.affects_constraints. The claim/metrics gap is deliberate:
 *   the constraint is CLAIMED as rope (a coordination problem requiring
 *   shared conceptual vocabulary) while the authored metrics describe the
 *   gate's actual historical operation, including its real differential costs
 *   — the engine measures the divergence rather than the author reconciling
 *   it.
 *
 * KEY AGENTS:
 *   - - hindu_algebraic_tradition: primary beneficiary (organized/identity_locked) — native scaffold, earliest operational entry, no experience of a gate at all
 *   - - islamic_algebraic_tradition: codifying beneficiary with agenda-setting secondary role (institutional/mobile) — standardized the convention and ran its westward transmission
 *   - - greek_geometric_algebra_tradition: primary cost-bearer (organized/identity_locked) — incompatible scaffold, centuries of computational and algebraic delay
 *   - - european_algorist_community: transition-generation payer turned beneficiary (organized/constrained) — bore bans and retraining, then captured the convention's full power
 *   - - non_positional_notation_cultures: excluded seat (moderate/trapped) — never confronted the latent structure, no seat in the process
 *   - - historians_of_mathematics: analytical observer — sees the full latency-plus-trigger structure from outside every practicing tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.38).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.25).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Scaffolding Gate on Zero-as-Number Operational Entry").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history of mathematics/philosophy of mathematics/conceptual history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, '6922c289-5e4b-4cc0-87ce-004b61503864').
narrative_ontology:cs_kernel_codification('6922c289-5e4b-4cc0-87ce-004b61503864', distributed).
narrative_ontology:cs_authority_grounding('6922c289-5e4b-4cc0-87ce-004b61503864', expertise).
narrative_ontology:cs_interpretation_layer_present('6922c289-5e4b-4cc0-87ce-004b61503864').
narrative_ontology:cs_reading_relation('6922c289-5e4b-4cc0-87ce-004b61503864', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('6922c289-5e4b-4cc0-87ce-004b61503864', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_axiom('6922c289-5e4b-4cc0-87ce-004b61503864', foundational, latent_structures_require_scaffolding_for_operational_thinkability).
narrative_ontology:cs_axiom_status(latent_structures_require_scaffolding_for_operational_thinkability, holdable).
narrative_ontology:cs_axiom_grounding('6922c289-5e4b-4cc0-87ce-004b61503864', latent_structures_require_scaffolding_for_operational_thinkability, empirically_contingent).
narrative_ontology:cs_axiom('6922c289-5e4b-4cc0-87ce-004b61503864', foundational, contact_triggers_recognition_rather_than_transmitting_content).
narrative_ontology:cs_axiom_status(contact_triggers_recognition_rather_than_transmitting_content, holdable).
narrative_ontology:cs_axiom_grounding('6922c289-5e4b-4cc0-87ce-004b61503864', contact_triggers_recognition_rather_than_transmitting_content, empirically_contingent).
narrative_ontology:cs_reference_frame('6922c289-5e4b-4cc0-87ce-004b61503864', scaffold_gated_latency).
narrative_ontology:cs_drift_state('6922c289-5e4b-4cc0-87ce-004b61503864', contemporary_cross_cultural_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6922c289-5e4b-4cc0-87ce-004b61503864', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, european_algorist_community).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, european_algorist_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sanskrit mathematical culture (the line running through Aryabhata, Brahmagupta, and Bhaskara) whose philosophical comfort with shunya — the void as an object of thought — and with algebraic abstraction over mere magnitudes supplied exactly the conceptual apparatus zero-as-number needed. Zero operated as a full operand in their computation centuries before anywhere else. Their scaffold was not an add-on acquired by choice; it grew out of religious-philosophical ground their practice was already fused with, so leaving it was never a live option and never needed to be.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    organized, generational, identity_locked, regional).

% The Abbasid-era translation movement and institutions like the House of Wisdom absorbed Hindu zero-grammar, codified it in Arabic algebra and administrative arithmetic (al-Khwarizmi's reckonings), and ran the standardization and westward transmission of the convention. They collected prestige, fiscal-administrative capability, and a large extension of algebraic reach. Unlike the Indian tradition they were adapters rather than natives of the scaffold, and they moved it freely across languages and empires.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition, agenda_setter).

% The tradition of magnitude-based geometric algebra (Euclidean construction, proportion theory) carried forward by Byzantine and Latin scholastic inheritors, equipped with alphabetic non-positional numerals and an Aristotelian inheritance hostile to treating nothing as a something. Its members bore centuries of computational delay and stunted algebraic development while the gate stayed shut for them. Exit meant abandoning the constructability-and-rigor ideal that constituted the tradition's self-understanding, which is why the lock held even as the costs mounted.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    organized, civilizational, identity_locked, regional).

% Merchants, reckoning-masters, and computists who took up Hindu-Arabic calculation from Fibonacci's Liber Abaci onward. The transition generations bore retraining costs, fraud suspicions, and municipal prohibitions such as Florence's 1299 ban on cipher numerals in account books. Once adopted, the convention repaid them with computational speed their abacist rivals could not match, and past a tipping point no practitioner could afford to opt out while competitors computed faster.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, european_algorist_community, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, european_algorist_community, beneficiary).

% Cultures computing with additive or alphabetic numeral systems — Roman, Greek alphabetic, Egyptian — never confronted the latent structure at all, because without place-value columns there was no hole for a zero to fill. Their scribal conventions locked their notation in place. They held no seat in the zero-entry process and had no vantage from which to see what they were missing.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, non_positional_notation_cultures, excluded,
    moderate, civilizational, trapped, continental).

% Reconstruct the entry process from manuscripts, transmission records, and counterfactual analysis of indigenous development paths. They adjudicate between rival explanations of why zero became operable when and where it did, and their documentary base sits outside every practicing tradition the process touched.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__hybrid_scaffolding_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_as_number_entry__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared operational grammar for treating zero as a full number — defined operations (adding it changes nothing, multiplying by it annihilates, dividing by it is barred) — so that positional notation becomes compositional: results can be written, combined, checked, and audited across practitioners instead of remaining private bookkeeping tricks.
% TRANSFER_FUNCTION: Moves computational capability and algebraic reach from the traditions holding compatible scaffolding outward to the wider mathematical commons, and moves transition costs onto traditions whose inherited frameworks were incompatible, which paid in delay, retooling, and surrendered algebraic ground.
% ABSENT_VOICES: Non-positional notation cultures never entered the conversation — the latent structure was invisible from their seats, so their silence reflects structural exclusion rather than consent. The Greek geometric-algebra tradition's own voice is permanently unavailable: it could have testified whether its framework could ever have generated zero-as-number indigenously, and no surviving document settles the counterfactual.
% DISAPPEARANCE_RATIONALE: If the scaffolding gate had vanished overnight — say, in 800 CE — positional-notation cultures everywhere would have operationalized zero immediately: decimal computation, negative numbers, and algebraic symbolism would have arrived centuries earlier across Eurasia, and the specific historical sequence of Indian priority, Islamic codification, and European resistance and late adoption would not exist. The developmental ordering of arithmetic and algebra demonstrably depended on where the gate stood open.
% FOUNDING_PROBLEM: Positional notation creates a structural hole: a place-value column holding nothing needs a mark that simultaneously means 'nothing here' and functions as an operand. Making that mark a NUMBER required solving the problem of operating on nothing — addition and subtraction with void, multiplication by void, division by void — and no notation alone answers it; a conceptual apparatus had to be supplied.
% FOUNDING_PROBLEM_CORROBORATION: Cross-cultural comparative notation studies and manuscript historiography — sources outside every benefiting tradition — attest both that the founding problem was real (European adoption consumed roughly four centuries of visible struggle, bans, and scholastic objection) and that it is now solved (every schoolchild operates zero mechanically). No living party depends on the gate remaining closed, and no beneficiary community's self-report is needed to establish either fact.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).
:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.38: the referent is the standing gating arrangement as this reading sees it — a real but UNCOLLECTED differential cost. The reading holds both that the gate had a structural-necessity component (some conceptual apparatus is unavoidable for operating on nothing, pulling epsilon down) and that its historical placement was contingent scaffolding distribution (pushing up), landing moderate on both. Suppression 0.25: the constraint owned no enforcement machinery — the gate ran on cognition, not policing; the Florence ban and scholastic suspicion were coercion aimed AT the convention by outsiders trying to hold the gate shut, which belongs to resistance, not to the constraint's own suppressive force. Theater_ratio 0.15: almost no performative maintenance; nothing was ritually defended because nothing was collecting. Accessibility_collapse 0.60: once the scaffolding move is seen, the placeholder-only alternative collapses for anyone doing advanced or compositional computation, but practical commerce survived on abaci and counters for centuries, so collapse is partial, not total. Resistance 0.50: real and historically documented (abacist guild rivalry, municipal bans, scholastic objection) but diffuse, uncoordinated, and ultimately ineffective. Temporal series run on one shared seven-point grid (458–1585) with both metrics authored at every point; base_extractiveness peaks at the 1299 ban era and declines as printing and Stevin's decimals consolidate the convention. A suppression_requirement series is deliberately omitted: the enforcement picture is static because the constraint never possessed enforcement capacity to build up or decay — the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience radically different constraints under the same structure. From the Hindu seat there was no gate: the scaffold was home territory, so the arrangement registers as pure facilitation, invisible as a constraint. From the Greek-inheritor seat the same structure is a wall that cost centuries and looked arbitrary — their rigor ideal was being counted as a defect. From the Islamic codifier seat it was an opportunity and an administrative instrument. From the analytical seat it is latency plus trigger. The engine computes per-seat classifications from the structural data (power, exit, role); the divergence between the identity_locked beneficiary and the identity_locked payer — same exit profile, opposite directionalities — is the sharpest edge in the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the Hindu and Islamic seats toward the subsidy end: the Hindu tradition's identity_locked exit deepens its beneficiary-side position (its practice was fused with the scaffold itself — it IS the scaffold, so no arbitrage-grade exit exists to pull d back toward symmetry). The Greek-scaffold inheritor derives a high target-side d, amplified by identity_locked: trapped, identity-fused targets sit nearer the full-target end than mobile ones, and this tradition could not abandon its constructability ideal even as the costs compounded. The European algorist community carries an explicit override to d=0.45: its dual payer/beneficiary declaration makes automatic derivation unstable (the engine could land it at either extreme depending on which declaration dominates), and its historical position was genuinely mixed — the transition generations bore bans, fraud suspicion, and retraining before capturing the convention's full benefit, a near-symmetric position with slight target tilt. The excluded non-positional cultures sit outside the extraction arithmetic entirely: they never entered, so they feed no directionality as targets or beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification guards against two symmetric mislabelings. Reading the gate as a mountain (the universal sibling's pull) would erase the differential costs and the contingency of timing — declaring natural what was historically distributed — and the declared victim set keeps the constructed component visible. Reading it as a snare (the contingent sibling's pull) would cast the barrier as imposed suppression with designed victims; but no seat captures the gains (gain_flow is affirmatively diffuse), suppression is low, and alternatives were never suppressed — the abacus persisted alongside algorism for three centuries, and Rome's numerals outlived the bans. The rope holds because the coordination function is genuine (shared zero-grammar makes positional arithmetic compositional and auditable), coercion is minimal, and participants are net beneficiaries. On mandatrophy: the founding problem is dead and the arrangement dissolved with it — the gate finished its work and ceased to bind anyone, which is why the R5 mismatch (dead status x world_rearranges verdict) is expected here rather than alarming; the zombie cross-check resolves clean against the low theater ratio and the absence of any capturing seat. The residual open question — whether scaffolding-gates of this kind form a recurring CLASS whose next instances are live somewhere now — is routed to the omega variables rather than smuggled into the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading — hybrid_scaffolding_reading — of kernel zero_as_number_entry. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Counterfactual analysis of indigenous-development paths combined with close reading of transmission documents. The disagreement is located in the counterfactual strength of the gate: contingent_thinkability_reading holds the gate was insurmountable without contact (no indigenous path at all); universal_discovery_reading holds there was no gate (availability unconditional, holder-priority incidental); this reading holds the gate was real for TIMING but not for POSSIBILITY (latent structure, scaffold-dependent operationalization).',
    'Classification shifts across readings: the universal reading computes a near-mountain profile with no victim set (lock-in becomes self-imposed ignorance, not borne cost); the contingent reading raises the barrier-side extraction and casts the European tradition as victim of an imposed wall; this reading yields a rope with a cost-bearing excluded tradition. Victim membership and epsilon both move with the reading adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: which reading of the zero-entry kernel this story instantiates and what siblings would change.').

omega_variable(
    latency_or_retrojection,
    'Was zero-as-number genuinely latent in positional notation''s internal logic, or is the ''latency'' retrojected from later formalization — that is, could positional arithmetic without zero-as-number have remained stable indefinitely, making availability weaker than this reading asserts?',
    'Formal reconstruction of positional arithmetic under a placeholder-only treatment of the empty column: determine whether the system forces contradiction, merely inconveniences, or runs indefinitely without pressure toward zero-as-operand.',
    'If the structure is genuinely forced, the necessity component strengthens, epsilon falls, and the reading tilts toward the universal sibling; if placeholder-only systems are stable, scaffolding contingency dominates, epsilon rises, and the gate looks more constructed than latent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latency_or_retrojection, empirical, 'Whether the mathematical-availability half of the hybrid premise is a real forcing or a retrospective projection.').

omega_variable(
    transmission_or_recognition_trigger,
    'Did Indo-Islamic-European contact transmit a concept (rules, symbols, treatises as content) or trigger recognition of a latent structure, as this reading holds?',
    'Compare pre-contact European encounters with the structure — Gerbertian abacus apices used digit symbols including a zero-mark around 980 CE without operational zero — against post-contact uptake. If Europeans were already touching the structure and failing to operationalize it until contact supplied the missing scaffold, the recognition-trigger model is confirmed.',
    'If contact transmitted content, the episode reduces to ordinary diffusion and the scaffolding gate loses its distinctive role; if contact triggered recognition, the gate is real and the hybrid reading''s core premise holds against both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_or_recognition_trigger, empirical, 'Mechanism of cross-cultural transfer: content-carriage versus structure-recognition.').

omega_variable(
    victim_status_dissipated_or_transferred,
    'Are Greek-scaffold inheritors victims of this constraint in the cost-bearing sense the structural delta assigns, or merely non-beneficiaries who failed to qualify — with their costs dissipated as historical delay rather than transferred to any collecting seat?',
    'Trace whether any actor''s position improved BECAUSE the Greek-scaffold tradition stayed locked: did algorist communities capture competitive advantage proportional to others'' lag, or did the locked tradition''s losses simply vanish as forgone development nobody harvested?',
    'If the costs functioned as competitive transfers, a tangled-rope flavor strengthens and the victim declaration carries extraction weight; if purely dissipated, the rope classification stands with a residual exclusion asymmetry that no seat profited from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_dissipated_or_transferred, conceptual, 'Whether the declared victim set bears transferred costs or only uncaptured delay.').

omega_variable(
    maya_zero_second_scaffold,
    'Does the Maya independent invention of zero in the Long Count calendar count as a second scaffolding path — supporting this reading''s claim that scaffolding, not unique Indian genius, opens the gate — or as evidence of unconditional availability, supporting the universal sibling?',
    'Comparative analysis of the operational scope of Maya zero: whether it generalized beyond calendrical positional arithmetic into general computation (division, arbitrary magnitudes) or remained confined to its original scaffold.',
    'If Maya zero was scaffold-bound and non-generalizing, the hybrid reading strengthens — scaffolding shapes not just entry but operational reach; if it was fully general, the universal reading strengthens and the gate thins toward nonexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maya_zero_second_scaffold, empirical, 'Independent Maya zero as a test case for the scaffolding thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 458, 1585).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t458, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 458, 0.1).
narrative_ontology:measurement(zero_tr_t628, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 628, 0.12).
narrative_ontology:measurement(zero_tr_t825, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 825, 0.12).
narrative_ontology:measurement(zero_tr_t1202, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1202, 0.18).
narrative_ontology:measurement(zero_tr_t1299, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1299, 0.22).
narrative_ontology:measurement(zero_tr_t1450, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1450, 0.18).
narrative_ontology:measurement(zero_tr_t1585, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1585, 0.15).

% Extraction over time
narrative_ontology:measurement(zero_be_t458, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 458, 0.3).
narrative_ontology:measurement(zero_be_t628, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 628, 0.32).
narrative_ontology:measurement(zero_be_t825, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 825, 0.35).
narrative_ontology:measurement(zero_be_t1202, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1202, 0.42).
narrative_ontology:measurement(zero_be_t1299, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1299, 0.48).
narrative_ontology:measurement(zero_be_t1450, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1450, 0.44).
narrative_ontology:measurement(zero_be_t1585, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1585, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__hybrid_scaffolding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the discovery of zero' decomposes into three readings of kernel zero_as_number_entry, per the epsilon-invariance principle — measuring the episode by indigenous-possibility, by transmission-necessity, or by unconditional-availability yields three different epsilons, three different victim sets, and three different classifications, so they are three constraints, not one. This file is the hybrid member: latent structure, scaffold-gated operationalization, contact as recognition-trigger. The upstream/downstream texture runs the other way from the usual family shape: the universal reading (highest confidence in the mathematical-availability claim) is cited as evidence BY the hybrid reading, which qualifies it with the scaffolding gate; the contingent reading competes with the hybrid on the counterfactual strength of that gate. Each member links the other two through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_as_number_entry__hybrid_scaffolding_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
