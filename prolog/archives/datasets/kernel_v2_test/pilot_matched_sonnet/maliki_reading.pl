% ============================================================================
% CONSTRAINT STORY: maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maliki_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Regional Practice and Public Interest Authority
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Maliki jurisprudential method privileges two sources that other Sunni
 *   schools treat with greater caution: the customary practice ('amal) of
 *   Medina as transmitted through continuous regional consensus, and public
 *   interest reasoning (maslaha mursala) unconstrained by explicit textual
 *   warrant. This reading of Islamic legal methodology creates a structural
 *   asymmetry: jurists embedded in Medinan legal lineages hold epistemic
 *   authority over transmitted practice, while jurists prioritizing hadith
 *   texts or independent reasoning face institutional marginalization. The
 *   constraint exhibits genuine coordination function — 'amal provides legal
 *   continuity where textual sources are ambiguous, and maslaha enables
 *   adaptive reasoning for novel cases — but this coordination is inseparable
 *   from extraction: the regional practice hierarchy concentrates
 *   interpretive authority in specific institutional lineages, and the
 *   flexibility of maslaha invocations creates opportunities for judicial
 *   discretion masked as public interest reasoning. The constraint is one
 *   reading of a contested kernel (usul al-fiqh method) — sibling readings
 *   (Hanafi, Shafi'i, Hanbali) privilege different source hierarchies and
 *   produce different beneficiary structures. Theater ratio (0.35) reflects
 *   moderate performative content: maslaha invocations sometimes function as
 *   post-hoc rationalizations for predetermined conclusions, but the
 *   constraint's coordination function remains substantial. Measurements show
 *   gradual drift toward higher extraction and theater as institutional
 *   gatekeeping intensifies and maslaha scope expands.
 *
 * KEY AGENTS:
 *   - Medinan Customary Practitioners: Primary beneficiaries (institutional/arbitrage) — 'amal authority channels epistemic legitimacy toward their transmitted practice
 *   - Regional Legal Authorities: Primary beneficiaries (institutional/arbitrage) — institutional position within Maliki madhhab grants interpretive authority
 *   - Maslaha-Invoking Jurists: Beneficiaries (organized/mobile) — maslaha framework provides flexibility for contemporary legal adaptation
 *   - Hadith Purist Scholars: Primary victims (powerless/identity_locked) — textualist commitment makes 'amal primacy an identity-level threat; cannot exit without abandoning scholarly framework
 *   - Textualist Jurists: Victims (moderate/constrained) — institutional pressure to defer to 'amal marginalizes text-based reasoning
 *   - Non-Medinan Practice Communities: Victims (powerless/trapped) — regional practice hierarchy structurally subordinates non-Medinan legal traditions
 *   - Comparative Legal Scholar: Analytical observer (analytical/analytical) — sees both coordination function and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maliki_reading, 0.38).
domain_priors:suppression_score(maliki_reading, 0.52).
domain_priors:theater_ratio(maliki_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(maliki_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(maliki_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maliki_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(maliki_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maliki_reading, tangled_rope).
narrative_ontology:human_readable(maliki_reading, "Maliki Jurisprudential Method: Regional Practice and Public Interest Authority").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, 'b090a4a3-9a69-4436-9717-86d0eab09f1f').
narrative_ontology:cs_kernel_codification('b090a4a3-9a69-4436-9717-86d0eab09f1f', formalized).
narrative_ontology:cs_authority_grounding('b090a4a3-9a69-4436-9717-86d0eab09f1f', lineage).
narrative_ontology:cs_interpretation_layer_present('b090a4a3-9a69-4436-9717-86d0eab09f1f').
narrative_ontology:cs_reading_relation('b090a4a3-9a69-4436-9717-86d0eab09f1f', maliki_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('b090a4a3-9a69-4436-9717-86d0eab09f1f', maliki_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('b090a4a3-9a69-4436-9717-86d0eab09f1f', maliki_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b090a4a3-9a69-4436-9717-86d0eab09f1f', foundational, transmitted_practice_epistemic_primacy).
narrative_ontology:cs_axiom_status(transmitted_practice_epistemic_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b090a4a3-9a69-4436-9717-86d0eab09f1f', transmitted_practice_epistemic_primacy, conventional).
narrative_ontology:cs_axiom('b090a4a3-9a69-4436-9717-86d0eab09f1f', foundational, maslaha_unrestricted_by_textual_warrant).
narrative_ontology:cs_axiom_status(maslaha_unrestricted_by_textual_warrant, holdable).
narrative_ontology:cs_axiom_grounding('b090a4a3-9a69-4436-9717-86d0eab09f1f', maslaha_unrestricted_by_textual_warrant, instrumental).
narrative_ontology:cs_reference_frame('b090a4a3-9a69-4436-9717-86d0eab09f1f', medinan_prophetic_practice_continuity).
narrative_ontology:cs_drift_state('b090a4a3-9a69-4436-9717-86d0eab09f1f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b090a4a3-9a69-4436-9717-86d0eab09f1f', '').
narrative_ontology:cs_kernel_id(maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, medinan_customary_practitioners).
narrative_ontology:constraint_beneficiary(maliki_reading, regional_legal_authorities).
narrative_ontology:constraint_beneficiary(maliki_reading, maslaha_invoking_jurists).
narrative_ontology:constraint_victim(maliki_reading, hadith_purist_scholars).
narrative_ontology:constraint_victim(maliki_reading, textualist_jurists).
narrative_ontology:constraint_victim(maliki_reading, non_medinan_practice_communities).
narrative_ontology:constraint_vindicates(maliki_reading, living_tradition_epistemic_authority).
narrative_ontology:constraint_vindicates(maliki_reading, regional_consensus_as_revelation_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves legal uncertainty in cases where Qur'anic and hadith texts are ambiguous or silent by appealing to transmitted Medinan practice as proxy for prophetic precedent, and enables adaptive legal reasoning for novel cases through public interest (maslaha) framework.
% TRANSFER_FUNCTION: Transfers interpretive authority from hadith-text scholars to regional-practice lineages; transfers epistemic legitimacy from textualist reasoning to customary-transmission claims; concentrates legal authority in Medinan institutional networks.
% ABSENT_VOICES: Hadith purist scholars who prioritize prophetic texts over regional custom are structurally marginalized — their textualist framework is subordinated by 'amal primacy. Non-Medinan practice communities whose regional customs lack the institutional authority of Medinan 'amal are excluded from the epistemic hierarchy. These voices are present in alternative madhahib (Shafi'i, Hanbali privilege hadith; Hanafi privileges non-Medinan reasoning) but marginalized within Maliki institutional discourse.
% DISAPPEARANCE_RATIONALE: If the Maliki method disappeared, the Islamic legal landscape would rearrange: Medinan institutional networks would lose epistemic authority; hadith-primacy scholars would gain relative influence; maslaha-based adaptive reasoning would require alternative justification; regional practice hierarchies would flatten. The constraint organizes real institutional arrangements and authority structures.
% FOUNDING_PROBLEM: Early Islamic legal development faced the problem of deriving law for cases not explicitly addressed in Qur'an or authenticated hadith, particularly in Medina where continuous practice from the prophetic era was claimed. The founding problem was: how to maintain legal continuity with prophetic precedent when textual sources are incomplete or contested?
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and historians of Islamic law attest the founding problem as live — legal uncertainty for novel cases remains, and 'amal provides continuity. Hadith purist scholars (Shafi'i, Hanbali traditions) contest the problem's status: they argue that authenticated hadith texts are sufficient and that 'amal claims often lack verifiable prophetic origin, making the 'founding problem' a retrospective justification for regional custom. Comparative legal scholars (analytical observers) note that the problem is real but that the Maliki solution (privileging Medinan practice) is one contested answer among several, not the only response to legal uncertainty.
narrative_ontology:disappearance_verdict(maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(maliki_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HADITH PURIST SCHOLAR (SNARE) — Identity-locked within textualist commitment; sees Maliki privileging of 'amal as systematic suppression of prophetic authority. Cannot exit without abandoning scholarly identity built on hadith primacy. Experiences maximum extraction: their epistemic framework is structurally subordinated by the constraint's operation.
constraint_indexing:constraint_classification(maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-MEDINAN JURIST (TANGLED ROPE) — Constrained by institutional pressure to defer to Medinan precedent while also benefiting from the maslaha framework's flexibility for local adaptation. Mixed experience: coordination function (public interest reasoning) exists alongside extraction (regional practice hierarchy marginalizes non-Medinan legal traditions).
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDINAN LEGAL AUTHORITY (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the constraint as pure coordination: 'amal authority solves the genuine problem of legal continuity with prophetic practice. Net beneficiary — the constraint channels epistemic authority toward this agent's institutional position.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MASLAHA REFORM COALITION (SCAFFOLD) — Organized jurists invoking maslaha mursala for contemporary legal adaptation see the constraint as transitional coordination with implicit sunset logic: as Islamic legal systems modernize, the specific privileging of Medinan practice becomes less necessary while the maslaha principle generalizes beyond regional boundaries. Mobile exit options through alternative jurisprudential frameworks.
constraint_indexing:constraint_classification(maliki_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination function (resolving legal uncertainty through transmitted practice) and asymmetric extraction (regional practice hierarchy creates epistemic gatekeeping). The maslaha framework provides real flexibility for public interest reasoning, but 'amal authority concentrates interpretive power in specific institutional lineages.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maliki_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from hadith purists and non-Medinan jurists through epistemic subordination, but extraction is not as severe as pure gatekeeping systems — maslaha framework provides some flexibility, and 'amal claims rest on plausible (if contested) transmission arguments. The value reflects real asymmetry in interpretive authority without total exclusion. Suppression (0.52): Moderate-high. Significant barriers to challenging 'amal authority include institutional pressure within Maliki legal education, regional consensus mechanisms that marginalize dissent, and identity costs for scholars whose training emphasized hadith primacy. But suppression is not total — alternative madhahib provide exit options, and internal Maliki debates over maslaha scope demonstrate some contestation space. Theater ratio (0.35): Moderate. Maslaha invocations sometimes function as post-hoc rationalizations (the conclusion is predetermined, public interest reasoning is theatrical justification), but the constraint retains substantial coordination function — many maslaha arguments represent genuine adaptive reasoning for novel cases. The theater has increased over the interval as maslaha scope expanded and institutional gatekeeping intensified. Accessibility collapse (0.15) and resistance (0.48): The constraint is not a natural law — alternative source hierarchies remain conceptually accessible (low collapse), and the constraint meets substantial resistance from textualist scholars (high resistance). These values contradict any mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across five perspectives. Hadith purists see pure extraction (snare) — 'amal authority systematically subordinates prophetic texts, and they cannot exit without abandoning their scholarly identity. Non-Medinan jurists see mixed coordination and extraction (tangled_rope) — maslaha provides real flexibility, but regional practice hierarchy marginalizes their traditions. Medinan authorities see pure coordination (rope) — 'amal solves the genuine problem of legal continuity, and they are net beneficiaries. The maslaha reform coalition sees transitional coordination (scaffold) — the constraint enables contemporary adaptation with implicit sunset logic as regional boundaries become less relevant. The analytical observer sees tangled_rope at civilizational scope — both genuine coordination function (transmitted practice resolves legal uncertainty) and asymmetric extraction (regional hierarchy concentrates interpretive authority). The gap is not 'which type is correct?' but 'which structural position are you measuring from?' All five classifications are legitimate perspectival readings of the same base properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Medinan legal authorities are primary beneficiaries with arbitrage exit options — they can navigate between Maliki institutional authority and broader Islamic legal discourse. The engine derives low d (beneficiary end) → low or negative chi. Hadith purist scholars are victims with identity_locked exit — their scholarly identity is constituted through textualist commitment, making 'amal primacy an existential threat they cannot escape without becoming different scholars. The engine derives high d (victim end) + identity_locked modulation → high chi. Non-Medinan jurists occupy a mixed position: constrained by institutional pressure to defer to Medinan precedent (victim status) but also benefiting from maslaha flexibility for local adaptation (partial beneficiary status). The engine derives moderate d → moderate chi, producing the tangled_rope classification. The maslaha reform coalition has mobile exit options (can adopt alternative jurisprudential frameworks) and partial beneficiary status (maslaha framework serves their adaptive goals), producing low-moderate d → low-moderate chi and scaffold classification. No directionality overrides are needed — the structural derivation from beneficiary/victim declarations and exit options captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope classification captures the structural reality: genuine coordination function (legal continuity through transmitted practice, adaptive reasoning through maslaha) is inseparable from asymmetric extraction (regional practice hierarchy, institutional gatekeeping). The coordination is real — 'amal provides continuity where texts are ambiguous, maslaha enables adaptation for novel cases. The extraction is also real — the constraint concentrates interpretive authority in specific lineages and creates opportunities for discretion masked as public interest reasoning. Neither pure rope (ignoring the epistemic hierarchy and gatekeeping) nor pure snare (ignoring the genuine coordination function) captures the structure. The tangled_rope classification, with its requirement for both beneficiaries and victims plus active enforcement, structurally enforces the recognition that coordination and extraction are coupled in this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Maliki privileging of ''amal one reading of a contested usul al-fiqh kernel, or a structurally distinct jurisprudential system?',
    'Cross-madhhab analysis of shared vs. divergent source hierarchies; historical tracing of methodological splits from common Medinan origins',
    'If one reading: sibling madhahib represent alternative interpretations of shared foundational commitments. If distinct system: each madhhab instantiates a separate constraint with different beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether Maliki method is a kernel reading or distinct system').

omega_variable(
    amal_transmission_reliability,
    'Does Medinan ''amal represent genuine unbroken transmission from prophetic practice, or retrospective construction of regional custom as revelation proxy?',
    'Historical-critical analysis of ''amal claims; comparison with early hadith transmission chains; identification of practices with no attested prophetic origin',
    'If genuine transmission: ''amal authority is epistemically grounded (coordination function dominates). If retrospective construction: ''amal is regional custom naturalized as divine authority (extraction dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_transmission_reliability, empirical, 'Epistemic status of ''amal transmission claims').

omega_variable(
    maslaha_constraint_boundary,
    'What structural limits, if any, constrain maslaha mursala invocations from collapsing into unconstrained judicial discretion?',
    'Doctrinal analysis of maslaha scope restrictions; case studies of rejected vs. accepted maslaha arguments; identification of implicit gatekeeping mechanisms',
    'If structurally constrained: maslaha is genuine public interest reasoning within defined bounds. If unconstrained: maslaha becomes cover for arbitrary judicial preference (theater ratio increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_constraint_boundary, empirical, 'Structural boundaries on maslaha invocations').

omega_variable(
    hanafi_relation_foreclosure,
    'Does Maliki ''amal primacy logically foreclose Hanafi ra''y (juristic reasoning) primacy, or do they coexist as competing regional traditions?',
    'Logical analysis of whether transmitted practice authority and independent reasoning authority can coexist in a single framework; historical analysis of Maliki-Hanafi institutional competition',
    'If forecloses: the reading_relations entry for hanafi_reading should be ''forecloses''. If coexists: should be ''coexists_with''. Affects cross-madhhab institutional dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_relation_foreclosure, conceptual, 'Logical relationship between ''amal and ra''y authority').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the transmitted legal method (usul al-fiqh as procedural framework), or the substantive claim that Medinan practice carries divine authority?',
    'Distinguish procedural disagreement (how to derive law) from substantive disagreement (what counts as authoritative source). If procedural: kernel is method, readings differ on source hierarchy. If substantive: kernel is the authority claim itself, readings differ on which regional tradition is authoritative.',
    'Procedural framing: cs_structure.kernel_codification = ''formalized'' (usul al-fiqh texts). Substantive framing: kernel_codification = ''distributed'' (no single text adjudicates which regional practice is authoritative). Current authoring assumes procedural framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Procedural vs substantive kernel framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_theater_founding, maliki_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(maliki_tr_t3, maliki_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(maliki_tr_t6, maliki_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(maliki_extract_founding, maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(maliki_be_t3, maliki_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(maliki_be_t6, maliki_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(maliki_su_t0, maliki_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(maliki_su_t3, maliki_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(maliki_su_t6, maliki_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Maliki reading is one of four sibling readings of the usul al-fiqh method kernel. Each reading has its own extractiveness value reflecting its specific beneficiary/victim structure. The readings are linked through institutional competition and cross-madhhab legal discourse. The Maliki reading's privileging of 'amal creates structural pressure on hadith-primacy readings (Shafi'i, Hanbali) by offering an alternative epistemic foundation, and its maslaha framework creates pressure on reasoning-restrictive readings by demonstrating adaptive capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
