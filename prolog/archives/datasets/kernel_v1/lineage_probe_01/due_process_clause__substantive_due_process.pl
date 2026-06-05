% ============================================================================
% CONSTRAINT STORY: due_process_clause__substantive_due_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_due_process_clause__substantive_due_process, []).

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
 *   constraint_id: due_process_clause__substantive_due_process
 *   human_readable: Substantive Due Process: Unenumerated Fundamental Liberties Against Majoritarian Regulation
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   Substantive due process is the constitutional doctrine that some
 *   liberties are so fundamental that no governmental process — however fair,
 *   however well-reasoned, however democratically enacted — can legitimately
 *   restrict them. The constraint arises from the Fourteenth Amendment's
 *   guarantee that no state shall deprive any person of 'liberty' without due
 *   process of law. This reading of the due process clause asserts that the
 *   clause has substance beyond procedure: it protects certain liberties from
 *   majoritarian regulation regardless of the procedural regularity of that
 *   regulation. The scope includes parental rights (Meyer v. Nebraska, Pierce
 *   v. Society of Sisters), marital privacy (Loving v. Virginia, Griswold v.
 *   Connecticut), contraceptive access (Griswold, Eisenstadt v. Baird),
 *   abortion (Roe v. Wade), and same-sex marriage (Obergefell v. Hodges). The
 *   constraint structures a fundamental institutional tension: federal courts
 *   are empowered to invalidate state legislation that violates unenumerated
 *   fundamental liberties, yet the Lochner precedent (overruling Lochner v.
 *   New York in 1937) delegitimized judicial second-guessing of economic and
 *   social legislation. This creates a doctrine that is simultaneously
 *   authoritative (courts regularly invoke substantive due process to protect
 *   liberties) and disreputable (courts cannot fully acknowledge the
 *   authority they are exercising without resurrecting Lochner's condemned
 *   reasoning). The measurement trajectory shows extractiveness increasing
 *   from 0.25 (1950s, minimal invocation) to 0.38 (2020s, robust invocation
 *   across multiple domains), and suppression increasing as courts face
 *   mounting criticism for judicial imposition of rights. The theater ratio
 *   remains moderate (0.35) because substantive due process retains genuine
 *   doctrinal work — courts are not merely performing deference; they are
 *   actively screening state regulations against fundamental liberty claims.
 *
 * KEY AGENTS:
 *   - Holders of Unenumerated Fundamental Liberties (powerless/trapped): Primary beneficiaries. Individuals claiming marriage, bodily autonomy, or parental rights that state legislatures have criminalized or restricted. Structurally trapped within state jurisdiction; cannot exit through democratic processes due to majority opposition. Depend on federal courts to recognize their liberty claim as fundamental.
 *   - Federal Courts (organized/constrained): Organized institutional actor with dual role. Benefit from authority to define fundamental liberties (coordination function). Constrained by Lochner repudiation, institutional vulnerability to state backlash, and the legitimacy crisis of claiming discovery authority for rights the Constitution does not enumerate.
 *   - Majoritarian State Legislatures (powerful/mobile): Beneficiaries from a coordination perspective. Substantive due process provides clear notice about which regulatory interests are off-limits, enabling efficient democratic governance in remaining domains. Victims from an extraction perspective: their authority to regulate moral questions (sexuality, reproduction, parental authority) is restricted by federal judicial assertion of unenumerated rights.
 *   - Fourteenth Amendment Framework (institutional/arbitrage): The constitutional container. Coordinates national protection of fundamental liberties; extracts authority from state legislatures.
 *   - The Lochner Precedent (institutional/constrained): Degraded authority. Courts invoke substantive due process selectively while disclaiming Lochner's economic version. Creates theatrical incoherence and instability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(due_process_clause__substantive_due_process, 0.38).
domain_priors:suppression_score(due_process_clause__substantive_due_process, 0.62).
domain_priors:theater_ratio(due_process_clause__substantive_due_process, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(due_process_clause__substantive_due_process, extractiveness, 0.38).
narrative_ontology:constraint_metric(due_process_clause__substantive_due_process, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(due_process_clause__substantive_due_process, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(due_process_clause__substantive_due_process, tangled_rope).
narrative_ontology:human_readable(due_process_clause__substantive_due_process, "Substantive Due Process: Unenumerated Fundamental Liberties Against Majoritarian Regulation").
narrative_ontology:topic_domain(due_process_clause__substantive_due_process, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(due_process_clause__substantive_due_process).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(due_process_clause__substantive_due_process, '2a924633-b203-428d-805a-1cbce5345949').
narrative_ontology:cs_kernel_codification('2a924633-b203-428d-805a-1cbce5345949', formalized).
narrative_ontology:cs_authority_grounding('2a924633-b203-428d-805a-1cbce5345949', lineage).
narrative_ontology:cs_interpretation_layer_present('2a924633-b203-428d-805a-1cbce5345949').
narrative_ontology:cs_reading_relation('2a924633-b203-428d-805a-1cbce5345949', due_process_clause__incorporation_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('2a924633-b203-428d-805a-1cbce5345949', due_process_clause__procedural_due_process, coexists_with).
narrative_ontology:cs_axiom('2a924633-b203-428d-805a-1cbce5345949', foundational, fundamental_liberty_beyond_majoritarian_revision).
narrative_ontology:cs_axiom_status(fundamental_liberty_beyond_majoritarian_revision, holdable).
narrative_ontology:cs_axiom_grounding('2a924633-b203-428d-805a-1cbce5345949', fundamental_liberty_beyond_majoritarian_revision, deontological).
narrative_ontology:cs_axiom('2a924633-b203-428d-805a-1cbce5345949', foundational, judicial_discovery_of_unenumerated_rights).
narrative_ontology:cs_axiom_status(judicial_discovery_of_unenumerated_rights, overridden).
narrative_ontology:cs_axiom_grounding('2a924633-b203-428d-805a-1cbce5345949', judicial_discovery_of_unenumerated_rights, empirically_contingent).
narrative_ontology:cs_reference_frame('2a924633-b203-428d-805a-1cbce5345949', unenumerated_fundamental_liberty_primacy).
narrative_ontology:cs_drift_state('2a924633-b203-428d-805a-1cbce5345949', contemporary_doctrinal_instability, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a924633-b203-428d-805a-1cbce5345949', '').
narrative_ontology:cs_kernel_id(due_process_clause__substantive_due_process, due_process_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(due_process_clause__substantive_due_process, holders_of_unenumerated_fundamental_liberties).
narrative_ontology:constraint_victim(due_process_clause__substantive_due_process, majoritarian_moral_regulation).
narrative_ontology:constraint_victim(due_process_clause__substantive_due_process, state_legislative_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED GROUP (SNARE) — Trapped within a state jurisdiction where majoritarian regulation suppresses their fundamental liberty through legislation. No exit short of relocation across state lines. Cannot challenge the law through ordinary democratic processes due to majority opposition. Maximum suppression, no arbitrage exit. The constraint operates as pure extraction: the state captures regulatory authority over spheres the disfavored group claims are fundamental.
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED RIGHTS-BEARING COMMUNITY (TANGLED ROPE) — Federal courts organized under the Fourteenth Amendment possess a dual role: enforcer of majoritarian will via procedural fairness, and guardian of unenumerated fundamental liberties against that same majoritarian will. Courts benefit from authority to define fundamental liberties (coordination function: legitimate judicial review of claimed rights). Courts are also constrained by the Lochner precedent (substantive due process is disreputable in modern doctrine) and face extraction pressure from state legislatures challenging the legitimacy of rights discovery. The constraint has genuine coordination function (judicial protection of unenumerated rights) and genuine asymmetric extraction (courts are caught between constitutional duty and institutional vulnerability).
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJORITARIAN LEGISLATIVE AUTHORITY (ROPE) — State legislatures view substantive due process as a coordination problem: how to align local moral regulation with federal constitutional constraints? The constraint enables coordination by providing a framework for legislatures to know which regulatory interests are fundamental (and thus off-limits) versus which can be democratically decided. Legislatures retain mobility: they can argue the liberty is not fundamental, can refactor the regulation's justification, or can litigate the constitutional boundary. Net beneficiary view: substantive due process gives legislatures clear notice about which regulatory domains are protected, enabling efficient democratic governance elsewhere.
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AUTHORITY (TANGLED ROPE) — The Fourteenth Amendment's liberty guarantee is the institutional container for substantive due process. It benefits from the legitimacy of constitutional supremacy (coordination: binding states to fundamental norms). It suffers extraction pressure from the Lochner precedent (judicial invalidation of economic and social legislation based on substantive due process was delegitimized in 1937). The constitutional framework has genuine coordination function (protecting fundamental liberties across state lines) and genuine extraction component (insulating certain regulatory choices from democratic revision). The tension is structurally irreducible: the amendment simultaneously coordinates national protection of fundamental liberties AND extracts authority from state legislatures.
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NATURAL LAW / ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational universalist perspective, some liberties (marriage, bodily autonomy, parental control) are so fundamental to human dignity that they are pre-political and inalienable. No process can legitimately extinguish them because they are not the state's to grant or take. This perspective views substantive due process as discovering and protecting what is inherently ours, not as the court imposing rights. The mountain classification derives from the view that these liberties are beyond majoritarian revision — they are natural law, not positive law. However, the structural data indicates this is a false summit: the beneficiaries and victims are empirically identifiable, suppression is high and contestable, and the constraint's extraction mechanism is institutional, not natural.
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LOCHNER PRECEDENT / DEGRADED AUTHORITY (PITON) — Substantive due process persists in constitutional doctrine despite the Lochner repudiation (1937). Courts invoke substantive due process to protect privacy, marriage, and bodily autonomy (Griswold, Loving, Roe, Obergefell) while simultaneously disclaiming Lochner's discredited economic substantive due process. The doctrine is partially inert — it survives on theatrical grounds (courts perform deference to the Fourteenth Amendment's text) while the actual mechanism (judicial skepticism of regulatory justifications) is only selectively applied. Theater ratio is moderate (0.35) because the doctrine retains genuine application in fundamental rights cases, but the Lochner shadow creates substantial performative hedging and doctrinal instability.
constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(due_process_clause__substantive_due_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(due_process_clause__substantive_due_process, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(due_process_clause__substantive_due_process, TR),
    TR >= 0.70.

:- end_tests(due_process_clause__substantive_due_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing over time. The constraint operates as an extraction mechanism for holders of unenumerated liberties (the state cannot take their claimed fundamental rights) but is also extractive against state legislative authority (courts invalidate democratically enacted regulations). The baseline extractiveness reflects the moderate scope of judicial invalidation — courts protect specific fundamental liberties but do not subject all state legislation to heightened scrutiny. The rising trajectory (0.25 → 0.38) captures the expansion of substantive due process from narrow application (Meyer, Pierce) to broader application (Griswold, Loving, Roe, Obergefell). Suppression (0.62): Moderate-high. For holders of disfavored liberties, suppression is severe: state legislatures can criminalize or restrict their claimed fundamental rights, and absent federal court intervention, they have no democratic exit. However, suppression is not total — some courts do recognize fundamental liberties; litigation pathways exist; federal protection has materialized in multiple domains. For state legislatures, suppression is moderate: they can argue the liberty is not fundamental, can refactor regulatory justifications, can litigate the boundary. Theater ratio (0.35): Low-moderate. The doctrine retains substantial functional content (courts do invalidate regulations on substantive due process grounds), but the Lochner shadow creates performative hedging. Courts frame rights discovery as constrained by history and tradition, even when stretching those constraints to reach contemporary liberty claims. The moderate theater reflects the tension between genuine doctrinal work and disreputable reasoning.
 *
 * PERSPECTIVAL GAP:
 *   The disfavored group sees snare (pure extraction via majoritarian suppression) but can hope for tangled_rope (mixed coordination and extraction) if federal courts recognize their liberty as fundamental. Majoritarian legislatures see rope (coordination: knowing which regulatory domains are protected) but experience tangled_rope (mixed coordination and constraint) when courts invalidate their regulations. Federal courts see tangled_rope (genuine protection function with institutional vulnerability and Lochner repudiation constraints). The constitutional framework sees tangled_rope (simultaneous coordination and extraction). The natural law observer risks seeing mountain (fundamental liberties as inherent and inalienable) but the structural data reveals this as a false summit: the constraint has identifiable beneficiaries, measurable suppression, and a contestable institutional mechanism. The Lochner precedent creates a piton perspective: the doctrine persists despite its formal repudiation, operating partially as theatrical compliance with Lochner's constraints while covertly applying substantive review.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. Holders of unenumerated liberties experience high d (close to 1.0, full target status): they are trapped with no arbitrage exit, and the constraint directly addresses their victimization. Federal courts experience moderate d (around 0.55–0.65): they benefit from coordination authority but face extraction pressure from the Lochner precedent and institutional vulnerability. Majoritarian legislatures experience low d (around 0.25–0.35) from a coordination perspective (they benefit from knowing the constitutional boundaries) but higher d from an extraction perspective (their authority is constrained). The analytical observer experiences high d (around 0.72): they see the full structural complexity and lack direct institutional stake. The derivation chain pulls d upward for trapped agents with victim status and downward for institutional agents with arbitrage or coordination benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint is ONE reading of the due_process_clause kernel. The substantive_due_process reading asserts that the Fourteenth Amendment protects certain liberties from majoritarian regulation regardless of process. The sibling readings — incorporation_doctrine (due process as conduit for enumerated rights) and procedural_due_process (due process as process) — present different institutional frameworks for constitutional authority. This reading is structurally coherent: it declares beneficiaries (holders of unenumerated fundamental liberties), victims (majoritarian regulatory authority and state legislatures), and a specific extraction mechanism (federal courts invalidate regulations that infringe fundamental liberty claims). The mandatrophy is resolved by acknowledging that substantive due process is a legitimately contested reading of the Fourteenth Amendment, not a singular constitutional truth. The constraint's classification as tangled_rope is robust — the constraint genuinely coordinates (provides clear notice of fundamental liberty boundaries) and genuinely extracts (federal courts override democratic regulation; state legislatures' authority is constrained). The false-summit risk is real: if the constraint naturalizes the judicial discovery of unenumerated rights as mountain-level inevitability, it obscures the contestable institutional choices embedded in substantive due process doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_liberty_definition_underdetermination,
    'What makes a liberty ''fundamental'' such that no process suffices to take it? Is the criterion rooted in historical tradition, in necessary conditions for human agency, or in democratic self-governance principles?',
    'Comparative analysis of how courts identify fundamental liberties (tradition-based in Michael H. v. Gerald D., agency-based in Planned Parenthood v. Casey, democracy-based in voting rights cases). Document which criteria survive across cases and which collapse.',
    'If historical tradition criterion: substantive due process becomes conservative (protects only deeply rooted practices; excludes novel liberty claims). If human agency criterion: substantive due process becomes expansive (protects any liberty necessary for autonomous self-definition; includes claims majoritarian review would reject). If democracy criterion: substantive due process becomes contradictory (protecting liberty from democratic revision via a democratic process — the Fourteenth Amendment itself).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_liberty_definition_underdetermination, conceptual, 'Criterion for identifying which liberties are fundamental').

omega_variable(
    lochner_shadow_extractiveness,
    'Does the Lochner repudiation reduce substantive due process''s extractiveness (by constraining judicial authority to invalidate legislation) or increase it (by forcing courts into incoherent doctrinal performances)?',
    'Empirical measure: compare invalidation rates in fundamental rights cases (post-1937) with pre-Lochner economic cases. Assess doctrinal coherence via cross-case reasoning patterns and precedential stability.',
    'If Lochner shadow reduces extractiveness: the constraint is primarily coordinating (courts follow Lochner constraints, states know the boundaries). If Lochner shadow increases extractiveness: the constraint is primarily extractive (courts appear to follow Lochner constraints while covertly applying substantive review; states cannot predict outcomes; legitimacy erodes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lochner_shadow_extractiveness, empirical, 'Whether Lochner repudiation reduces or increases extractiveness').

omega_variable(
    kernel_reading_contest_underdetermination,
    'This constraint is ONE reading (substantive_due_process) of a contested kernel (due_process_clause). The three readings — incorporation_doctrine, procedural_due_process, substantive_due_process — present irreducibly different institutional frameworks for what ''due process'' means. Does this constraint''s classification (tangled_rope) depend on which sibling reading is adopted?',
    'Comparative constraint authoring: generate separate constraint stories for each sibling reading using identical base structural data (the same beneficiaries, victims, and extractiveness metrics). Compare the three classifications. If all three yield the same type (e.g., all tangled_rope), the classification is robust to reading choice. If they diverge, document which reading choice produced which classification and why.',
    'If robust: substantive due process classification stands independently of the kernel contest. If reading-dependent: the constraint story embeds an undefended choice among the sibling readings. The engine''s committer-frame logic will flag this as an unresolved axiom debate, requiring explicit axiom declarations to resolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_underdetermination, conceptual, 'Whether classification is robust to choice among kernel readings').

omega_variable(
    rights_discovery_vs_rights_invention,
    'Is the judicial invocation of unenumerated fundamental liberties discovering pre-existing rights (natural law reading, supporting mountain classification) or inventing new doctrinal categories for majoritarian suppression (positivist reading, supporting snare/tangled_rope classification)?',
    'Jurisprudential analysis: examine whether courts frame rights claims as discovery (reiterating pre-existing universal principles) or invention (articulating new constitutional meaning). Track whether the reasoning is backward-looking (rooted in history/tradition) or forward-looking (responding to contemporary liberty claims). Assess whether discovered rights are claimed as universals or particularities.',
    'If discovery frame: substantive due process approximates mountain (unenumerated liberties are natural law, pre-political, inalienable). If invention frame: substantive due process is tangled_rope or snare (courts are imposing majoritarian constraints via the appearance of discovering universal rights). This is the critical axis for false-summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_discovery_vs_rights_invention, conceptual, 'Whether unenumerated rights are discovered or invented').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(due_process_clause__substantive_due_process, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(due__be_t0, due_process_clause__substantive_due_process, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(due__be_t50, due_process_clause__substantive_due_process, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(due__be_t100, due_process_clause__substantive_due_process, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(due__su_t0, due_process_clause__substantive_due_process, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(due__su_t50, due_process_clause__substantive_due_process, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(due__su_t100, due_process_clause__substantive_due_process, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(due_process_clause__substantive_due_process, enforcement_mechanism).
narrative_ontology:affects_constraint(due_process_clause__substantive_due_process, due_process_clause__incorporation_doctrine).
narrative_ontology:affects_constraint(due_process_clause__substantive_due_process, due_process_clause__procedural_due_process).

% DUAL FORMULATION NOTE:
% The substantive_due_process reading decomposes from the contested due_process_clause kernel. This story models the constraint structure when the clause is read to protect unenumerated fundamental liberties from majoritarian regulation. The sibling readings (incorporation_doctrine, procedural_due_process) model alternative readings of the same Fourteenth Amendment text. All three constraint stories share the kernel (the due process clause) but instantiate different institutional frameworks, beneficiary/victim relationships, and extraction mechanisms. Each reading has its own ε value reflecting empirical estimates of how much the doctrine actually constrains legislative authority and how much it protects claimed liberties. Substantive due process is characterized here as tangled_rope (ε=0.38) because it protects fundamental liberties (coordination) while overriding democratic regulation (extraction), and the Lochner repudiation creates institutional tension. The incorporation reading emphasizes the enumerated-rights protection function. The procedural reading emphasizes fair-process coordination without substantive constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
