% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading): Polygamy Doctrinally Valid, Manifesto as Duress-Suspension
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the continuationist reading of the
 *   contested kernel 'divine marriage command.' The continuationist reading
 *   holds that polygamy remains doctrinally valid within the LDS/Mormon
 *   tradition; the 1890 Manifesto that suspended the practice is interpreted
 *   as a prudential response to federal coercion (the Edmunds Act and
 *   subsequent anti-polygamy statutes), not as a doctrinal rescission or new
 *   revelation replacing the original command. This reading is held by
 *   fundamentalist LDS splinter communities (FLDS, others) that claim
 *   continuity with pre-Manifesto doctrine and authority. The mainstream LDS
 *   institution maintains doctrinal distinction while enforcing monogamy,
 *   creating a structural tension: the institution cannot claim the original
 *   command was false without revision, yet it cannot sustain practice
 *   without violation of federal law. The continuationist reading extracts
 *   value by positioning fundamentalist splinters as the true inheritors of
 *   revealed doctrine while the mainstream institution is portrayed as
 *   compromised by political pressure. The reading also enables the
 *   fundamentalist community to maintain theological legitimacy despite legal
 *   persecution. The constraint exhibits tangled rope structure: genuine
 *   coordination function (defining doctrinal boundaries within a
 *   revelation-based tradition) combined with asymmetric extraction (the
 *   fundamentalist splinters extract legitimacy while bearing suppression).
 *   The measurement data shows rising theater ratio and extraction over the
 *   interval (t=0 to t=100, representing roughly 1890–1990 and beyond): as
 *   federal pressure was applied and the mainstream institution solidified
 *   its enforcement of monogamy, the performative content of maintaining
 *   'doctrinal validity' while preventing practice increased, and the cost to
 *   fundamentalist practitioners (legal vulnerability, community isolation)
 *   rose.
 *
 * KEY AGENTS:
 *   - Fundamentalist LDS Communities (FLDS, others): Primary beneficiaries of the continuationist claim (organized/constrained) — doctrinal legitimacy justifies community continuity and practices
 *   - Fundamentalist Practitioners: Primary victims (powerless/identity_locked) — identity constituted through belief in doctrinal validity; face legal suppression and community isolation
 *   - Mainstream LDS Institutional Authority: Secondary beneficiary (institutional/arbitrage) — maintains doctrinal authority while shedding legal liability through Manifesto distinction
 *   - Federal Legal Authority: Constrained agent (institutional/constrained) — enforces monogamy law but needs mainstream institutional cooperation
 *   - Doctrinal Continuity Claimants: Beneficiaries across fundamentalist splinters (organized/constrained) — benefit from claim to reveal­ed doctrine and pre-Manifesto succession
 *   - Monogamist Mainstream Members: Victims indirectly (moderate/constrained) — bear burden of doctrinal ambiguity and institutional contradiction between claimed and enforced doctrine
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural extraction mechanism (denial of revision) masked by theological naturalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading): Polygamy Doctrinally Valid, Manifesto as Duress-Suspension").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '358bc274-fc70-4592-a0c7-75939a60a5d4').
narrative_ontology:cs_kernel_codification('358bc274-fc70-4592-a0c7-75939a60a5d4', fixed_text).
narrative_ontology:cs_authority_grounding('358bc274-fc70-4592-a0c7-75939a60a5d4', lineage).
narrative_ontology:cs_interpretation_layer_present('358bc274-fc70-4592-a0c7-75939a60a5d4').
narrative_ontology:cs_reading_relation('358bc274-fc70-4592-a0c7-75939a60a5d4', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('358bc274-fc70-4592-a0c7-75939a60a5d4', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('358bc274-fc70-4592-a0c7-75939a60a5d4', foundational, prudential_suspension_preserves_doctrinal_validity).
narrative_ontology:cs_axiom_status(prudential_suspension_preserves_doctrinal_validity, holdable).
narrative_ontology:cs_axiom_grounding('358bc274-fc70-4592-a0c7-75939a60a5d4', prudential_suspension_preserves_doctrinal_validity, theological).
narrative_ontology:cs_axiom('358bc274-fc70-4592-a0c7-75939a60a5d4', foundational, federal_coercion_external_to_doctrine).
narrative_ontology:cs_axiom_status(federal_coercion_external_to_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('358bc274-fc70-4592-a0c7-75939a60a5d4', federal_coercion_external_to_doctrine, conventional).
narrative_ontology:cs_reference_frame('358bc274-fc70-4592-a0c7-75939a60a5d4', unambiguous_divine_command_regime).
narrative_ontology:cs_drift_state('358bc274-fc70-4592-a0c7-75939a60a5d4', contemporary_post_manifesto_institutional_divergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('358bc274-fc70-4592-a0c7-75939a60a5d4', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_succession_communities).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, doctrinal_continuity_claimants).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, monogamist_mainstream_institutional_authority).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, legal_compliance_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNDAMENTALIST PRACTITIONER (SNARE) — Identity-locked within the continuationist reading. Exit would require abandoning both the theological legitimacy claim and the community identity constituted through it. Structurally mobile (can leave the fundamentalist community geographically) but identity-fused with the doctrinal claim that polygamy remains valid. Bears maximum suppression: legal vulnerability, social isolation, operational security burden. No genuine exit option at identity level.
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUNDAMENTALIST SUCCESSION COMMUNITY (TANGLED ROPE) — Organized agents (Warren Jeffs' FLDS, other continuationist splinters) benefit from the doctrinal claim (theirs is the legitimate continuation of revelation) while bearing significant suppression (legal prosecution, resource isolation, membership control). The constraint coordi­nates community identity and doctrine while extracting labor, reproductive compliance, and capital. Constrained exit: leaving the community is possible but high-cost (family severing, identity dissolution, economic vulnerability).
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAINSTREAM LDS INSTITUTIONAL AUTHORITY (ROPE) — Experiences the continuationist reading as a coordination problem to be solved through doctrinal distinction and membership boundary-setting. The 1890 Manifesto and subsequent disaffiliation of fundamentalist splinters enable the mainstream institution to maintain institutional legitimacy while abandoning polygamy practice. Net position: beneficiary (retains authority to define doctrine while shedding legal liability). Exit options: high — can revise doctrine, can excommunicate splinters, can reframe narrative. Extracted value flows toward the institution (doctrinal authority, historical continuity claim, membership consolidation).
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIVINE REVELATION INVARIANTIST (MOUNTAIN) — A civilizational-scope perspective that treats the original divine command (plural marriage) as an immutable theological premise. The claim is that divine commands do not change in substance, only in applicability. From this view, the constraint (the doctrinal validity of polygamy) is fixed by divine will. However, this classification is a false summit: the theological premise that divine commands cannot be rescinded without explicit new revelation is itself contestable and benefits the continuationist community by naturalizing their reading as logically necessary.
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL LEGAL AUTHORITY (TANGLED ROPE) — Constrains the practice through anti-polygamy statutes while needing mainstream LDS institutional cooperation for effective governance. Coordi­nates civil order (constraint function) while extracting compliance and legitimacy from the institution. The mainstream LDS chooses the Manifesto as a solution that maintains theological authority while satisfying legal requirements. Constrained exit: escalating the conflict through imprisonment and asset seizure has its own costs.
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OFFICIAL DISAFFILIATION PROTOCOL (PITON) — The institutional machinery for excommunicating fundamentalist splinters is largely performative. The mainstream institution has no direct enforcement mechanism over splinters (they are not members); the excommunication ritual reaffirms institutional doctrine for the mainstream community rather than actually preventing fundamentalist practice. Theater ratio high: the protocol persists through institutional tradition (reaffirming doctrinal boundaries) rather than through functional control. The constraint degraded over time as fundamentalist communities organized independently.
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FUNDAMENTALIST EXIT COALITION (SCAFFOLD) — Organized agents (activist groups, law enforcement interventions, women's rights advocates) are constructing alternative pathways for fundamentalist members to exit (safe houses, legal support, identity transition support). The scaffold has a sunset clause: as exit infrastructure matures and becomes normalized, the suppressive power of community isolation diminishes. Mobile exit options (support structures reducing cost of departure) convert the constraint from snare to mobile-path constraint. Scaffold extraction χ is low because the coalition has agency and sees an explicit exit-building goal.
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DOCTRINAL STABILITY VIEW (SNARE) — From a civilizational analytical context, the constraint represents the instability of doctrinal frameworks under legal pressure. The original command (plural marriage) creates binding expectations; suspending it requires either new revelation (substitutionist reading) or doctrinal distinction (this reading claims suspension is prudential, not doctrinal). The analytical observer sees that the continuationist reading bears maximum structural extraction — it must suppress evidence that the institution has functionally revised doctrine while claiming doctrinal immutability. The denial of internal revision is itself the extraction mechanism: practitioners must maintain cognitive dissonance between the claimed doctrine (polygamy valid) and the institutional practice (enforced monogamy).
constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_marriage_command__continuationist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, TR),
    TR >= 0.70.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The continuationist reading extracts legitimacy from fundamentalist communities (they are the 'true inheritors' of revealed doctrine) while positioning them as illegal and subordinate. The reading also extracts benefit for the mainstream institution (doctrinal authority without practice liability). The measurement trajectory (0.35 → 0.62) reflects that extraction has intensified as federal enforcement increased — the continuationist claim became more valuable to splinters as their legal vulnerability rose. Suppression (0.72): High and rising. Fundamentalist practitioners face legal prosecution, community isolation, loss of mainstream institutional protection, operational security burdens, and social stigma. The suppression is structural (legal system, economic barriers) and partly internalized (identity lock: exit requires abandoning belief in doctrinal legitimacy). The rising suppression_requirement measurement reflects that maintaining the continuationist practice became more costly over the interval. Theater ratio (0.68): Moderate-high and rising. The mainstream institution's continued claim that polygamy is 'doctrinally valid' while enforcing monogamy is largely performative — the doctrinal claim is maintained for internal narrative coherence and historical legitimacy, not because the institution is prepared to reinstitute practice. The rising theater reflects that as time passed without reinstitution, maintaining the claim required increasingly explicit doctrinal distinction (it's valid but suspended indefinitely, it's valid but applicable only in celestial marriage, etc.). The claim becomes more theatrical as it becomes less credible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies perspectival gaps at the level of institutional theology. The fundamentalist practitioner sees the continuationist reading as liberatory (reveals true doctrine, legitimizes their practice) while the analytical observer sees it as extractive (maintains cognitive dissonance and community isolation). The mainstream institution sees monogamous doctrine as achieved (the Manifesto succeeded) while the fundamentalist splinter sees it as suspended (still pending when conditions change). The federal authority sees monogamy as enforced while the continuationist theology claims enforcement is merely suppressing practice of a still-valid doctrine. These gaps are not bridgeable by adding data — they are structural. The same doctrinal claim functions as legitimacy for one group and suppression for another. The false-summit perspective (mountain classification) risks naturalizing the theological framing as inherent law when structural analysis reveals it as a social mechanism for managing competing legitimacy claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by perspective based on structural relationship to the constraint. Fundamentalist practitioners (powerless/identity_locked) experience maximum d ≈ 0.95: they are fully targeted by suppression while deriving identity legitimacy from the reading — the constraint isolates them while binding them. Fundamentalist communities (organized/constrained) experience moderate d ≈ 0.50: they benefit from the doctrinal claim (legitimacy, community continuity) but bear legal suppression and resource isolation. Mainstream institutional authority (institutional/arbitrage) experiences low d ≈ 0.15: they are net beneficiary (doctrinal authority, legal compliance, membership consolidation) with high exit options (can revise doctrine or disaffiliate further). Federal authority (institutional/constrained) experiences moderate d ≈ 0.55: they enforce monogamy (benefit) but require mainstream cooperation and bear enforcement costs. The analytical observer (analytical/analytical) experiences high d ≈ 0.72: observing the structural extraction mechanism requires seeing through the theological naturalization, placing the observer at the border of acceptable discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (ambiguity about whether this is coordination or extraction) is resolved by recognizing that the continuationist reading serves both functions simultaneously. It coordinates within the fundamentalist community (defines who belongs, what is legitimate) while extracting from the mainstream institution (positioning it as compromised, deriving relative legitimacy from the claim to true doctrine). The constraint cannot be reduced to pure coordination (rope) because the fundamental function is to establish asymmetric legitimacy: the fundamentalist reading is correct, the mainstream reading is corruption. It cannot be reduced to pure extraction (snare) because the reading genuinely does coordinate community identity and doctrine. Tangled rope is the precise classification: mixed coordination-extraction with asymmetric distribution. The mandatrophy resolves to: this is a coordination mechanism (doctrinal definition) deployed as an extraction mechanism (legitimacy hierarchy) within a contested kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prudential_vs_doctrinal_boundary,
    'Can prudential suspension of a command remain consistent with the doctrine that the command is eternally valid? Where is the boundary between ''temporary suspension under duress'' and ''doctrinal rescission''?',
    'Comparative theology across revelation-based traditions (Islamic jurisprudence on abrogation, Christian typology on law/gospel, Jewish law on pikuach nefesh overrides); historical analysis of whether continuationist splinters ever claim conditions for reinstitution of practice',
    'If boundary is precise: continuationist reading is logically coherent. If boundary is ambiguous: the reading collapses into substitutionist (denial of rescission is itself rescission). If splinters claim permanent conditions for reinstitution: reading is revealed as aspirational without functional mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prudential_vs_doctrinal_boundary, conceptual, 'Philosophical boundary between prudential suspension and doctrinal rescission').

omega_variable(
    false_summit_naturalization,
    'Is the divine marriage command itself a natural law grounded in theology, or is the claim to immutability a socially constructed legitimacy device deployed by agents who benefit from doctrinal continuity?',
    'Structural analysis: does the mainstream institution benefit from maintaining a claim to doctrinal immutability while functionally abandoning the practice? Comparative case: how do competing religious traditions handle similar doctrinal conflicts between original claims and institutional evolution?',
    'If natural law: the constraint is a genuine theological limit, not a social mechanism. If constructed: the mountain classification is a false summit and the constraint is tangled_rope throughout. The beneficiary structure (fundamentalist splinters claiming legitimacy through doctrinal continuity) suggests constructed naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether divine command immutability is theological natural law or legitimacy naturalization').

omega_variable(
    cognitive_dissonance_sustainability,
    'How long can the mainstream institution sustain the cognitive dissonance between claimed doctrine (polygamy eternally valid) and enforced practice (monogamy mandatory)? What are the stability limits?',
    'Longitudinal analysis of doctrinal statements, practice enforcement, and institutional messaging; comparison to other traditions managing similar contradictions; interview data on how members resolve the contradiction',
    'If sustainable indefinitely: the institution can maintain the continuationist reading operationally. If unstable: pressure toward either substitutionist revision (explicit new doctrine) or coercion_visibility (acknowledged suspension under duress) will increase over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_dissonance_sustainability, empirical, 'Sustainability of doctrinal-practice dissonance within mainstream institution').

omega_variable(
    splinter_legitimacy_claim_basis,
    'Do continuationist fundamentalist communities claim theological legitimacy because they (a) read the existing doctrine correctly, or (b) maintain institutional continuity with pre-Manifesto authority structures? Is the claim to legitimacy doctrinal or genealogical?',
    'Textual analysis of fundamentalist theology; historical reconstruction of institutional lineage; comparison between groups that claim direct revelation and groups that claim unbroken succession from early leadership',
    'If doctrinal: both mainstream and fundamentalist can claim the same doctrine. If genealogical: the legitimacy claim rests on who holds organizational authority, not doctrine. This determines whether the readings are competing interpretations or competing authority claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(splinter_legitimacy_claim_basis, empirical, 'Basis of fundamentalist legitimacy claim: doctrinal or genealogical').

omega_variable(
    kernel_revision_unacknowledged,
    'Is the continuationist reading an instance of a contested kernel (divine marriage command with multiple readings) or is it an instance of false-summit mountain (a claim to immutability masking functional revision)?',
    'Meta-analysis: does this story belong in the kernel frame (multiple live readings of a fixed commitment) or in the false-summit frame (naturalized claim hiding contingent revision)? The distinction turns on whether the institution treats the disagreement as interpretive (our reading is correct, theirs is wrong) or as illegitimate (they are outside the framework entirely).',
    'If contested kernel: this story is one reading among multiple. If false summit: this story documents naturalization of institutional change. The committer frame assumes contested kernel; if the structural analysis reveals false summit, the story''s frame is miscalibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_revision_unacknowledged, conceptual, 'Whether constraint belongs in contested kernel frame or false-summit frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_cont_theater_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dmc_cont_theater_t50, divine_marriage_command__continuationist_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement(dmc_cont_theater_t100, divine_marriage_command__continuationist_reading, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(dmc_cont_extract_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dmc_cont_extract_t50, divine_marriage_command__continuationist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(dmc_cont_extract_t100, divine_marriage_command__continuationist_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dmc_cont_suppress_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dmc_cont_suppress_t50, divine_marriage_command__continuationist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(dmc_cont_suppress_t100, divine_marriage_command__continuationist_reading, suppression_requirement, 100, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (continuationist) of the contested kernel DIVINE_MARRIAGE_COMMAND. The kernel encompasses three structurally distinct constraints with different ε values and beneficiary/victim structures: continuationist (ε=0.58, this story), substitutionist (ε≈0.28, mainstream institutional), coercion_visibility (ε≈0.45, intermediate). Each reading is a separate constraint story. They are linked via network edges and via the kernel_context commentary. The ε-invariance principle requires that the three readings be separate stories because they have different observable measures of doctrinal validity and different structural incentives for each group holding the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
