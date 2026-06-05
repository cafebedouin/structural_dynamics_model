% ============================================================================
% CONSTRAINT STORY: british_constitution__modern_judicialization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_constitution__modern_judicialization, []).

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
 *   constraint_id: british_constitution__modern_judicialization
 *   human_readable: British Constitutional Judicialization (1998-Present)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   Since 1998, the British constitution has undergone profound
 *   judicialization through three structural changes: the Human Rights Act
 *   1998 (incorporating the European Convention on Human Rights into domestic
 *   law), devolution (establishing Scottish Parliament, Welsh Assembly,
 *   Northern Ireland Assembly, and English Regional Assemblies), and the
 *   Constitutional Reform Act 2005 (establishing the Supreme Court). These
 *   instruments have transformed the British constitution from a system of
 *   political self-restraint and parliamentary sovereignty grounded in
 *   uncodified conventions toward a hybrid regime of statutory constraints
 *   and judicial review. The constraint operates as a Tangled Rope: it
 *   genuinely coordinates rights protection and devolved power-sharing
 *   (coordination function) while simultaneously extracting parliamentary
 *   sovereignty and traditional constitutional doctrine (asymmetric
 *   extraction). The constraint is maintained through active enforcement via
 *   judicial review and statutory precedent. Suppression is moderate-high:
 *   actors committed to pure parliamentary supremacy find the constraint
 *   inescapable without formal constitutional amendment, yet alternatives
 *   exist for those willing to incur political cost. The theater ratio has
 *   risen from 0.45 (at 1998 implementation) to 0.62 (by 2013), reflecting
 *   that the constitutional language persists (parliamentary sovereignty,
 *   Crown prerogative, convention) while the structural mechanisms of
 *   enforcement have shifted from political to judicial. This reading of the
 *   kernel—the modern_judicialization reading—is one of five live political
 *   positions on what the British constitution is. It coexists with
 *   constitutional_conventions (the uncodified usage view),
 *   foundational_charters (the Magna Carta lineage view),
 *   parliamentary_supremacy_statutes (the Crown-in-Parliament supremacy
 *   view), and revolution_settlement (the 1688-1701 settlement view). No
 *   single reading forecloses another in contemporary British politics,
 *   though the modern_judicialization reading has achieved institutional
 *   entrenchment through court structure and statutory grounding.
 *
 * KEY AGENTS:
 *   - Rights-Claimants and Civil Society: Primary beneficiaries (institutional/arbitrage) — direct access to judicial enforcement of rights without legislative action; gain protection against majoritarian erosion
 *   - Judicial Institutions: Primary beneficiary (institutional/arbitrage) — Supreme Court and lower courts gain enhanced authority, expanded jurisdiction, and legitimacy as constitutional arbiters; pure extraction flow toward judiciary
 *   - Parliamentary Sovereignty Purists: Primary victim (powerless/trapped) — traditional constitutional doctrine of Crown-in-Parliament supremacy has been subordinated without formal amendment; cannot exit or restore doctrine without large-scale constitutional reform
 *   - Devolved Legislatures: Secondary victim (moderate/constrained) — gain autonomy through devolution but face judicial constraints on competence and increasing Westminster/Supreme Court scrutiny; mixed experience of coordination and extraction
 *   - Executive and Central Government: Secondary victim (institutional/constrained) — loses discretionary authority in rights adjudication while gaining control over devolved competences via judicial review; mixed extraction
 *   - Political Parties and Parliament: Organized victim (organized/constrained) — loss of majoritarian discretion through judicial constraints; suppression moderate but increasing as precedent hardens
 *   - Constitutional Conventions Repository: Institutional observer (institutional/constrained) — pre-1998 enforcement mechanism (political practice) displaced by statute and case law; conventions degraded to historical reference (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_constitution__modern_judicialization, 0.38).
domain_priors:suppression_score(british_constitution__modern_judicialization, 0.48).
domain_priors:theater_ratio(british_constitution__modern_judicialization, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_constitution__modern_judicialization, extractiveness, 0.38).
narrative_ontology:constraint_metric(british_constitution__modern_judicialization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(british_constitution__modern_judicialization, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_constitution__modern_judicialization, tangled_rope).
narrative_ontology:human_readable(british_constitution__modern_judicialization, "British Constitutional Judicialization (1998-Present)").
narrative_ontology:topic_domain(british_constitution__modern_judicialization, "political/legal/constitutional").

domain_priors:requires_active_enforcement(british_constitution__modern_judicialization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_constitution__modern_judicialization, 'eb230e59-d284-43a0-b365-488c4f1e7fac').
narrative_ontology:cs_kernel_codification('eb230e59-d284-43a0-b365-488c4f1e7fac', distributed).
narrative_ontology:cs_authority_grounding('eb230e59-d284-43a0-b365-488c4f1e7fac', lineage).
narrative_ontology:cs_interpretation_layer_present('eb230e59-d284-43a0-b365-488c4f1e7fac').
narrative_ontology:cs_reading_relation('eb230e59-d284-43a0-b365-488c4f1e7fac', british_constitution__constitutional_conventions, influences).
narrative_ontology:cs_reading_relation('eb230e59-d284-43a0-b365-488c4f1e7fac', british_constitution__foundational_charters, coexists_with).
narrative_ontology:cs_reading_relation('eb230e59-d284-43a0-b365-488c4f1e7fac', british_constitution__parliamentary_supremacy_statutes, coexists_with).
narrative_ontology:cs_reading_relation('eb230e59-d284-43a0-b365-488c4f1e7fac', british_constitution__revolution_settlement, influences).
narrative_ontology:cs_axiom('eb230e59-d284-43a0-b365-488c4f1e7fac', foundational, rights_are_judicially_enforceable).
narrative_ontology:cs_axiom_status(rights_are_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('eb230e59-d284-43a0-b365-488c4f1e7fac', rights_are_judicially_enforceable, deontological).
narrative_ontology:cs_axiom('eb230e59-d284-43a0-b365-488c4f1e7fac', foundational, sovereignty_is_constrained_by_procedure).
narrative_ontology:cs_axiom_status(sovereignty_is_constrained_by_procedure, holdable).
narrative_ontology:cs_axiom_grounding('eb230e59-d284-43a0-b365-488c4f1e7fac', sovereignty_is_constrained_by_procedure, empirically_contingent).
narrative_ontology:cs_reference_frame('eb230e59-d284-43a0-b365-488c4f1e7fac', political_constitutionalism_pre_1998).
narrative_ontology:cs_drift_state('eb230e59-d284-43a0-b365-488c4f1e7fac', contemporary_post_2013, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb230e59-d284-43a0-b365-488c4f1e7fac', '').
narrative_ontology:cs_kernel_id(british_constitution__modern_judicialization, british_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_constitution__modern_judicialization, rights_claimants).
narrative_ontology:constraint_beneficiary(british_constitution__modern_judicialization, judicial_institutions).
narrative_ontology:constraint_victim(british_constitution__modern_judicialization, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_victim(british_constitution__modern_judicialization, traditional_political_settlement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARLIAMENTARY SOVEREIGNTY PURIST (SNARE) — Traditional constitutional doctrine of Crown-in-Parliament supremacy has been structurally subordinated to judicial review without formal amendment. The purist finds the constraint inescapable: cannot exit parliamentary governance; cannot restore the doctrine; must accept adjudication of political decisions. Maximum extraction: the traditional constitutional structure is neutered while its nominal form persists.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVOLVED LEGISLATURES (TANGLED ROPE) — Scottish Parliament, Welsh Senedd, and Northern Ireland Assembly gain genuine legislative authority through devolution (coordination function) while facing judicial constraints on their competence and increasing scrutiny from Westminster and the Supreme Court. Mixed experience: significant autonomy alongside structural extraction. Exit is costly but theoretically possible (constitutional renegotiation).
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHTS-CLAIMANTS AND CIVIL SOCIETY (ROPE) — Human Rights Act 1998 and subsequent jurisprudence enable direct judicial enforcement of rights without legislative action. The constraint solves a genuine coordination problem: how to protect rights against majoritarian erosion. Beneficiaries of the judicialization mechanism; low experienced extraction; can arbitrage between legislative and judicial forums. Net coordination gain.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE AND CENTRAL GOVERNMENT (TANGLED ROPE) — Gains from enhanced judicialization of devolved competences (Westminster can veto devolved legislation via court review) while losing discretionary executive authority in rights adjudication. Mixed extraction: control over devolution offset by judicial constraints on prerogative powers. Exit is nominal — cannot withdraw from constitutional system but can work within the constraints.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL INSTITUTIONS (ROPE) — Supreme Court establishment (2009) and enhanced judicial review powers provide institutional authority, prestige, and expanded jurisdiction. Judicial review becomes a coordination mechanism for settling political disputes that Parliament leaves ambiguous. Pure beneficiary position: extraction flows toward courts; low suppression on judiciary. Can arbitrage between activism and restraint depending on political moment.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POLITICAL PARTIES AND PARLIAMENT (TANGLED ROPE) — Organized actors experience the constraint as both coordination mechanism (judicial clarity on constitutional limits) and extraction (loss of parliamentary sovereignty and majoritarian discretion). Suppression moderate: parties can propose constitutional amendment but face high coordination costs and precedent resistance. Suppression_requirement has grown over interval as judicial precedent hardens.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL CONVENTIONS REPOSITORY (PITON) — Pre-1998 regime of uncodified constitutional conventions (prime ministerial restraint, ministerial accountability, Crown neutrality) persists nominally but has been largely displaced by statute and case law. Conventions remain in discussion but are enforced through judicial precedent rather than political practice. Theater ratio high: the language of convention persists; the mechanism has shifted. Piton classification reflects degradation of convention-as-binding-usage into convention-as-historical-reference.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SCAFFOLD) — The judicialization of 1998-2009 represents a transitional constitutional settlement: movement from pure political constitutionalism (conventions + parliamentary sovereignty) toward codified constitutional law (statute, charter, case precedent). Theater ratio moderate (0.62 in base properties) reflects the hybrid: formal statutory grounding in Human Rights Act and devolution statutes alongside continuity of parliamentary supremacy language. Exit via further constitutional reform (codified constitution, written bill of rights) or reversion to political constitutionalism remains theoretically available, though politically costly.
constraint_indexing:constraint_classification(british_constitution__modern_judicialization, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_constitution__modern_judicialization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_constitution__modern_judicialization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_constitution__modern_judicialization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_constitution__modern_judicialization, TR),
    TR >= 0.70.

:- end_tests(british_constitution__modern_judicialization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The judicialization mechanism extracts parliamentary sovereignty and traditional political constitutionalism from the pure sovereignty doctrine, but this extraction is not total — Parliament retains the ability to amend statutes (though amendment costs are high due to constitutional entrenchment norms). The extraction is frontloaded in the 1998-2005 period (extractiveness rises from 0.22 to 0.38 over 15 years) and then plateaus, consistent with institutional lock-in after initial imposition. Suppression (0.48): Moderate-high. Actors committed to pure parliamentary sovereignty face real barriers to exit: formal constitutional amendment requires supermajority consensus (practically impossible in current UK politics), and reversal of Human Rights Act and devolution would trigger large-scale political crisis. However, suppression is not total — political actors can work within the system (Tangled Rope perspective for Parliament), and theoretical alternatives exist (departure from European Convention, recentralization of devolved powers). Theater ratio (0.62): Moderate-high. The constraint exhibits significant performative content: parliamentary sovereignty language persists in legislative debates and official doctrine even though judicial review has become the operative enforcement mechanism. Conventions are still discussed as binding even though they are now enforced through statute and case precedent rather than political practice. The theater has increased over the interval (0.45 → 0.62) as the system has matured — early judicialization was presented as radical innovation; later periods treat it as constitutional normalcy. Claimed type (Tangled Rope): Justified by presence of genuine coordination function (rights protection, devolved autonomy) + asymmetric extraction (parliamentary sovereignty subordinated) + active enforcement (judicial review mechanism) + requires_active_enforcement flag.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a core diagnostic property: the same judicialization mechanism appears as pure coordination (Rope) to rights-claimants, mixed coordination-extraction (Tangled Rope) to multiple institutional actors, pure extraction (Snare) to parliamentary sovereignty purists, degradation of old mechanisms (Piton) to conventions custodians, and transitional settlement (Scaffold) to the analytical observer. The perspectival gaps are large: rights-claimants experience the constraint as liberation; parliamentary sovereignty purists experience it as violation; judicial institutions experience it as empowerment; devolved legislatures experience it as partial autonomy with extraction. No single classification captures all perspectives. The gap reveals that judicialization is not a unidimensional change but a restructuring of the constitutional settlement that benefits some agents (rights-claimants, courts, devolved regions as autonomy-seekers) while extracting from others (parliamentary sovereignty doctrine, executive discretion, parliamentary majoritarian power). The divergence between perspectives is permanent: there is no unified constitutional vision that satisfies all stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its classification from the agent's power level, time horizon, exit options, and beneficiary/victim status in the constraint. Parliamentary sovereignty purists (powerless/trapped) experience maximum extraction because they believe the constraint is unjust and illegitimate, yet they cannot exit or reverse it without massive political mobilization. Judicial institutions (institutional/arbitrage) experience low extraction because they are beneficiaries with exit options (courts can exercise restraint or activism depending on political moment). Devolved legislatures (moderate/constrained) experience mixed extraction because they gain autonomy (beneficiary function) while losing sovereignty (victim function), and they face moderate-high exit costs (constitutional renegotiation). Rights-claimants (institutional/arbitrage) experience low extraction because they are beneficiaries with arbitrage options (can use legislative or judicial forums as strategic advantage). The executive (institutional/constrained) experiences mixed extraction with moderate exit costs (can propose constitutional amendment or work within constraints, but both options are costly). Political parties (organized/constrained) experience moderate extraction with moderate-high exit costs (suppression_requirement increases over time as judicial precedent hardens). The Piton perspective on conventions reflects degradation of the enforcement mechanism from political practice to statutory precedent, while the Scaffold perspective on the analytical observer reflects the constraint as a transitional settlement rather than a stable constitutional equilibrium.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution for this constraint is structural pluralism: all eight perspectives are legitimate readings of the judicialization constraint from their respective positions, and no single type is 'the' correct classification. The Snare classification for parliamentary sovereignty purists is correct from their frame (they experience non-negotiable extraction). The Rope classification for rights-claimants is correct from their frame (they experience coordination gain). The Tangled Rope classifications for institutional actors reflect genuine mixed extraction-coordination experiences. The Piton for conventions reflects genuine degradation of the pre-1998 enforcement mechanism. The Scaffold for the analytical observer reflects the transitional nature of the current settlement. The mandatrophy is resolved not by choosing a single type but by recognizing that the constraint is a contested kernel with five live readings, and each reading generates different classifications depending on observer position. The true constraint is the multiplicity of readings, not any single reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statute_vs_convention_boundary,
    'Where does judicialization end and statutory constitutionalism begin? Are the Human Rights Act and devolution statutes constraints imposed by courts on Parliament, or are they constitutive of a new parliamentary-judicial partnership?',
    'Historical analysis of parliamentary intent in 1998 legislation; examination of subsequent amendments and judicial interpretation patterns; comparison of UK model to other hybrid constitutional systems (Canada, Australia)',
    'If statutes are Parliament''s own constitution-making (self-imposed restraint): classification shifts toward Rope for Parliament and beneficiary institutions. If statutes are externally imposed (courts claiming interpretive authority): classification shifts toward Snare for parliamentary sovereignty doctrine. Current taxonomy assumes hybrid (Tangled Rope), but the boundary is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_vs_convention_boundary, conceptual, 'Boundary between statutory self-restraint and judicially-imposed limitation').

omega_variable(
    devolution_as_coordination_or_extraction,
    'Does devolution represent genuine power-sharing (coordination gain) or Westminster control disguised as autonomy (extraction masked as federalism)?',
    'Measurement of judicial reversals of devolved legislation; analysis of reserved vs devolved competence boundaries; historical tracking of devolved legislature satisfaction and sovereignty movements; cross-national comparison (Canada, Australia, US federalism)',
    'If genuine coordination: devolved legislatures classify as Rope beneficiaries; extraction minimal. If Westminster control: devolved legislatures classify as Snare victims; judicialization is extraction mechanism. Current assessment (Tangled Rope for devolved legislatures) assumes mixed extraction and autonomy; resolution would clarify the proportion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(devolution_as_coordination_or_extraction, empirical, 'Whether devolution constitutes genuine power-sharing or disguised central control').

omega_variable(
    judicial_restraint_sustainability,
    'Will the Supreme Court and lower courts maintain self-restraint in political cases (narrowing the extractive reach of judicialization), or will judicial review inevitably expand into more contested political terrain?',
    'Longitudinal analysis of judicial review case volume and scope; examination of rejected/granted judicial review petitions by subject matter; judicial opinion language tracking restraint rhetoric; comparison to other supreme court systems and their scope creep patterns',
    'If restraint holds: judicialization remains Tangled Rope (coordination + modest extraction). If judicial scope expands: extractiveness rises toward Snare thresholds; suppression increases. Current measurement shows modest theater_ratio increase (0.45 → 0.62), consistent with early-stage scope creep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_restraint_sustainability, empirical, 'Sustainability of judicial self-restraint versus inevitable scope creep').

omega_variable(
    reading_contest_foreclosure,
    'Does the modern_judicialization reading logically foreclose any sibling reading of the british_constitution kernel, or do all five readings coexist as live political positions?',
    'Examination of contemporary political contestation: Are ''constitutionalists'' committed to exactly one reading, or do advocates shift between readings depending on political moment? Can a single actor hold two readings simultaneously? Is there a logical structure that makes one reading''s core premise incompatible with another''s?',
    'If readings foreclose each other: the kernel is unstable and will collapse toward dominant reading (likely modern_judicialization given institutional entrenchment). If readings coexist: the kernel remains contested indefinitely, and constitutional politics will perpetually replay the contest. Current assessment (coexists_with and influences relations) assumes contestation is permanent; resolution reveals degree of institutional stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether sibling readings foreclose each other or coexist permanently').

omega_variable(
    theater_ratio_directional_ambiguity,
    'Does the theater ratio of 0.62 reflect performative retention of parliamentary supremacy language (obscuring judicial authority), or performative adoption of constitutional language by a political system that remains fundamentally sovereignty-based?',
    'Discourse analysis of parliamentary debates, judicial opinions, and political speeches on constitutional questions (2010-2025); identification of which actors invoke which language; tracking of whether invoked constitutional principles are actually dispositive in political decisions',
    'If theater is sovereignty-language cover: suppression and extractiveness are underestimated (political actors perform constraint they don''t believe in). If theater is constitutionalism-language aspiration: suppression and extractiveness are overestimated (political actors are gradually adopting genuine constitutionalism). Current assessment treats theater as symmetrical; directional resolution would adjust base metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_directional_ambiguity, empirical, 'Direction of theatrical gap: cover for old sovereignty or adoption language for new constitutionalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_constitution__modern_judicialization, 1998, 2013).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(judic_tr_t0, british_constitution__modern_judicialization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(judic_tr_t5, british_constitution__modern_judicialization, theater_ratio, 5, 0.52).
narrative_ontology:measurement(judic_tr_t10, british_constitution__modern_judicialization, theater_ratio, 10, 0.6).
narrative_ontology:measurement(judic_tr_t15, british_constitution__modern_judicialization, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(judic_be_t0, british_constitution__modern_judicialization, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(judic_be_t5, british_constitution__modern_judicialization, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(judic_be_t10, british_constitution__modern_judicialization, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(judic_be_t15, british_constitution__modern_judicialization, base_extractiveness, 15, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(judic_su_t0, british_constitution__modern_judicialization, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(judic_su_t5, british_constitution__modern_judicialization, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(judic_su_t10, british_constitution__modern_judicialization, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(judic_su_t15, british_constitution__modern_judicialization, suppression_requirement, 15, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_constitution__modern_judicialization, enforcement_mechanism).
narrative_ontology:affects_constraint(british_constitution__modern_judicialization, british_devolution_competence_boundary).
narrative_ontology:affects_constraint(british_constitution__modern_judicialization, european_convention_incorporation_enforcement).
narrative_ontology:affects_constraint(british_constitution__modern_judicialization, parliamentary_sovereignty_doctrine_erosion).

% DUAL FORMULATION NOTE:
% The modern_judicialization constraint is upstream of specific disputes over devolved competence (Scotland/Wales autonomy), European Convention enforcement (rights adjudication), and parliamentary sovereignty doctrine (Crown prerogative). Each of these has its own ε and perspectives. The judicialization constraint represents the overall structural shift toward judicial authority; the downstream constraints represent specific instantiations of that shift in particular domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(british_constitution__modern_judicialization, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
