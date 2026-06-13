% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope: Progressive Textualist Reading
 *   domain: constitutional/political/civil-rights
 *
 * SUMMARY:
 *   The progressive textualist reading of the equality clause treats it as a
 *   principle genuinely present in the founding text but with application
 *   scope historically bounded to the categories contemplated in 1787 (or
 *   1868 for the 14th Amendment). Under this reading, expansion to cover
 *   excluded groups—women, enslaved people, non-property-owners, racial
 *   minorities—requires explicit constitutional amendment, not judicial
 *   reinterpretation. This reading sits between strict originalism (equality
 *   applies only to 1787 categories, period, even via amendment) and
 *   expansive universalism (courts can discover that equality applies to all
 *   humans without amendment). The constraint embodies a specific theory of
 *   constitutional legitimacy: scope changes are binding when they clear the
 *   supermajority democratic bar (Article V), not when courts assert them.
 *   The reading generates real costs for those seeking faster judicial
 *   remedies and real benefits for those who won amendment victories—their
 *   expanded scope is treated as supremely legitimate precisely because it
 *   cleared the amendment gate.
 *
 * KEY AGENTS:
 *   - Progressive textualist judges: gate keepers of judicial restraint; interpret original scope as binding, amendment as the path to expansion.
 *   - Democratic amendment coalition: the beneficiary seat; their victories (13th, 14th, 19th, 26th Amendments) are legitimized as constitutional rather than judicial overreach.
 *   - Historically excluded groups: the payer seat; they wait for amendment while lacking judicial remedies; they bear the cost of the supermajority requirement.
 *   - Judicial remediation seekers (civil rights litigators): payers; they lose access to courts as a route to faster equality expansion.
 *   - Originalist judges: excluded; they object that textualists allow amendment to reshape the clause at all.
 *   - Expansive universalist scholars: excluded; they claim courts should read equality universally without amendment requirement.
 *   - Constitutional scholars (neutral): observers tracking whether the amendment process is genuinely functional or gridlocked.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.42).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope: Progressive Textualist Reading").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional/political/civil-rights").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '7f7d9005-9d58-4aba-8f38-35f339399230').
narrative_ontology:cs_kernel_codification('7f7d9005-9d58-4aba-8f38-35f339399230', formalized).
narrative_ontology:cs_authority_grounding('7f7d9005-9d58-4aba-8f38-35f339399230', lineage).
narrative_ontology:cs_interpretation_layer_present('7f7d9005-9d58-4aba-8f38-35f339399230').
narrative_ontology:cs_reading_relation('7f7d9005-9d58-4aba-8f38-35f339399230', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('7f7d9005-9d58-4aba-8f38-35f339399230', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_axiom('7f7d9005-9d58-4aba-8f38-35f339399230', foundational, original_scope_binding).
narrative_ontology:cs_axiom_status(original_scope_binding, holdable).
narrative_ontology:cs_axiom_grounding('7f7d9005-9d58-4aba-8f38-35f339399230', original_scope_binding, conventional).
narrative_ontology:cs_axiom('7f7d9005-9d58-4aba-8f38-35f339399230', foundational, amendment_authority_legitimates_expansion).
narrative_ontology:cs_axiom_status(amendment_authority_legitimates_expansion, holdable).
narrative_ontology:cs_axiom_grounding('7f7d9005-9d58-4aba-8f38-35f339399230', amendment_authority_legitimates_expansion, conventional).
narrative_ontology:cs_reference_frame('7f7d9005-9d58-4aba-8f38-35f339399230', text_bounded_principle_open_to_amendment).
narrative_ontology:cs_drift_state('7f7d9005-9d58-4aba-8f38-35f339399230', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f7d9005-9d58-4aba-8f38-35f339399230', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_amendment_coalition).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, textualist_legal_community).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, historically_excluded_groups).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_remediation_seekers).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, constitutional_amendment_supremacy).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, supermajority_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the equality clause as originally bounded but open to deliberate revision through constitutional amendment. They enforce this reading by declining to read modern expansions into the text via judicial doctrine, instead remanding scope-expansion questions to the electoral/amendment process. They claim fidelity to the text's original boundaries while preserving the amendment mechanism as the legitimate path to expansion. Their power is real but constrained: they can decline to expand, but they cannot prevent amendments from succeeding, and they must honor amendments once ratified.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, progressive_textualist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Gains legitimacy from the constraint: their expansion victories (13th Amendment abolition of slavery, 14th Amendment birthright citizenship and equal protection, 19th Amendment suffrage, 26th Amendment voting age) are treated as supremely legitimate precisely because they cleared the supermajority threshold. The textualist reading validates their achievements as constitutional amendments, not as judicial overreach, and makes future expansions equally dependent on their coalition-building success. They can exit by choosing not to pursue amendments, or mobilize by building supermajorities.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_amendment_coalition, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of waiting: between moments when they lack explicit textual inclusion and the (difficult, multi-decade) moment when sufficient democratic consensus accumulates for amendment. They cannot exit the jurisdiction. They lack the political power to unilaterally force the amendment process. They experience the constraint as exclusion sustained by legal doctrine that treats their rights as contingent on democratic supermajority rather than self-evident or judicially discoverable. Their exit options are severely constrained: they can migrate (leave the nation), organize for amendment (requires political coalition power they often lack), or litigate in state courts (which may or may not adopt the same reading).
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, historically_excluded_groups, payer,
    powerless, biographical, trapped, national).

% Civil rights litigators, scholars, and advocates who believe courts possess authority to read equality principles expansively without amendment. They bear the cost of the constraint by seeing judicial remedies foreclosed and being redirected to the much slower, harder amendment process. They can exit via forum choice (try state courts, international human rights bodies, legislative lobbying) but federal constitutional remedies are structurally constrained by the textualist reading. Their exit to state courts is imperfect: state constitutions may have their own textualist judges, and state remedies do not bind other states.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_remediation_seekers, payer,
    moderate, biographical, constrained, national).

% Would advocate for an even narrower reading: equality applies only to the scope explicitly contemplated in 1787/1868 without amendment authority changing it. They are partially captured by the textualist reading (which accepts original boundaries) but object to the amendment-openness; they form a coalition with textualists against expansive judicial readings but diverge on whether amendments can reshape the clause's meaning. They are excluded from the constraint because their preferred reading (frozen scope, no amendment authority) is not the governing doctrine.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, originalist_judges, excluded,
    institutional, generational, constrained, national).

% Argue that equality is self-evident and applies to all humans regardless of historical exclusions and amendment status. They are systematically excluded from the constraint's framing: the textualist reading treats their core claim (that the court can discover universal equality) as judicial overreach. They influence discourse, publish extensively, and lobby for legislative/amendment action, but they are foreclosed from the governing legal doctrine. Their exit options are constrained: they can move to jurisdictions adopting their reading (some state courts or foreign jurisdictions), but federal constitutional doctrine is not accessible to them through this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, expansive_universalist_scholars, excluded,
    powerful, generational, constrained, national).

% The constitutional amendment mechanism itself: 2/3 of both houses or 2/3 of state legislatures to propose, 3/4 of states to ratify. Not an agent, but the procedural mechanism through which the constraint enforces its boundary between judicial interpretation and democratic consent. The amendment process is the legitimacy source for scope expansion under this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, article_v_amendment_process, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__progressive_textualist, article_v_amendment_process).

% Track how the reading operates in practice: whether it genuinely enables amendment-driven expansion or whether amendment gridlock makes the reading functionally exclusionary despite its nominal openness. They measure whether the supermajority requirement is a real procedural bottleneck or a formal gate that amendments consistently clear. Their analytical position is mobile: they can publish critiques, influence legal education, but cannot directly change doctrine or amendment outcomes.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_scholars_neutral, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, democratic_amendment_coalition).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates legal meaning-making: it establishes supermajority democratic amendment as the sole legitimate mechanism for expanding equality's scope, displacing competing mechanisms (judicial discovery, international law, unenumerated rights doctrine). This solves a coordination problem about whose authority determines constitutional meaning — the textualist reading answers: the text as originally bounded, revisable only by democratic supermajority. The coordination function is genuine: without some stable answer to 'who decides when equality expands,' the constitutional system fragments (courts read expansively, originalists freeze the text, states diverge). The amendment-gate provides a unified legitimacy structure.
% TRANSFER_FUNCTION: Transfers authority from courts (which could read equality expansively under alternative doctrines) to the electoral coalition capable of securing 2/3 + 3/4 supermajorities. Transfers legitimacy to amendment victors (their scope expansion is treated as constitutive constitutional change, not overreach). Transfers costs to historically excluded groups awaiting amendment (they lose access to judicial remedies and depend on political coalition success—a difficult, multi-decade process). Transfers interpretive constraint to all subsequent judges (they must honor amendment-created new boundaries, not read beyond them). The net flow: political supermajorities receive legitimacy authority; excluded groups pay the cost of waiting for that supermajority to form.
% ABSENT_VOICES: Originalist judges (wanting narrower, unamendable readings, fully freezing the clause's scope) are partially absent—they are heard in dissent but not in the governing doctrine. Expansive universalist scholars (claiming courts should discover universal equality without amendment) are systematically excluded—their core claim (judicial authority to read equality universally) is foreclosed by the textualist reading. Historically excluded groups before amendment (their interests are excluded from the decision about which mechanism governs expansion)—they cannot vote on the textualist reading itself, and they cannot directly force the amendment process; they can only organize within it. International human rights bodies and foreign legal traditions are excluded from the framing (the constraint is purely internal to U.S. constitutional interpretation).
% DISAPPEARANCE_RATIONALE: If the progressive textualist constraint vanished overnight, courts would revert to either expansive interpretation (reading equality universally, discovering that it applies to all humans) or pure originalism (treating equality as frozen in 1787/1868 scope with no amendment rewriting). The amendment process would lose its role as the legitimacy-conferring mechanism for scope expansion. Scope expansion would become either judicial decree (which would delegitimize prior non-amended expansions like the 14th Amendment if read retroactively as overreach, or validate them as precedent) or impossible (if originalism prevails and amendments are treated as unable to reshape the clause's meaning). Constitutional interpretation would reorganize around a different legitimacy principle: either courts as interpreters, originalists as boundary-keepers, or some hybrid.
% FOUNDING_PROBLEM: Early Constitution contained slavery, property restrictions on suffrage, and gender exclusions while professing equality principles ('We the People,' 'all men are created equal'). Founders/framers faced an irresolvable tension: the text claims universal equality but its application was historically bounded. The constraint emerged to solve the legitimacy problem: how to honor the principle's universality, respect the original historical boundaries, AND allow deliberate revision without treating either as judicial fiction. The founding problem is: on what authority does the meaning of 'equality' expand when it is applied to categories not originally included?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and textualist scholars attest the constraint's founding: the 13th Amendment (abolition of slavery) and the 14th Amendment (birthright citizenship and equal protection) were explicitly marketed as amendments, not as judicial discoveries of equality's scope within the unamended text. This suggests the prior understanding was that equality's scope was bounded and required amendment to expand. Progressive legal scholars attest the problem is live (courts in the 1960s-1980s faced questions about how far to read equality—to race, gender, sexual orientation—without amendment). Originalist scholars contest whether the problem was ever real, treating the constraint as a capitulation to judicial overreach (arguing that original equality was always universally applicable, or alternatively, that it was properly bounded and amendment cannot change that). Expansive universalist scholars attest the problem is misconceived (equality is self-evident and applies to all humans regardless of amendment status). The problem's status is not corroborated outside the legal academy and judiciary; it is internal to constitutional interpretation practice and reflects disagreement about the legitimacy of different interpretive mechanisms.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint imposes real costs—excluded groups cannot access judicial remedies, remediation seekers lose a forum—but the costs are not pure extraction; they are the price of a legitimacy structure that treats amendment as supremely binding. Suppression is moderate (0.38) because the constraint works partly through legal doctrine (judges declining to read expansively) and partly through political structure (the amendment process is genuinely difficult, requiring 2/3 + 3/4 supermajorities). Theater is low (0.22) because the constraint's operation is relatively transparent: judges explain they are not reading beyond original scope; amendments are public supermajority votes; the mechanism is visible. Accessibility collapse is moderate-high (0.65) because once the textualist reading is established, alternatives (judicial expansion, unamended application) become legally unavailable, though politically contestable. Resistance is moderate-high (0.58) because originalists resist from one side (wanting narrower, unamendable scope) and universalists resist from the other (wanting judicial expansion). The measurement series spans 60 years to capture amendment cycles and doctrinal drift. Extractiveness shows slight upward drift (0.35→0.42) as courts more consistently enforce the textualist boundary without making exceptions. Theater shows slight upward drift as rhetorical stakes around amendment rise. Suppression is stable because the judicial doctrinal suppression of expansive reading is consistent across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The amendment-coalition and democratic-process seats compute the constraint as rope (genuine coordination: legitimate scope change requires democratic supermajority, a real gate against tyranny of the courts). The historically-excluded-groups and remediation-seeker seats compute it as snare (exclusion from judicial remedies, forced to wait for political consensus that may never arrive, genuine victims of the supermajority requirement). The textualist judges compute it as necessary separation of powers: honoring the text and the amendment process. The engine should compute seat-specific types reflecting these asymmetries: the beneficiaries' seat sees coordination; the payers' seats see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic amendment coalition: d~0.25 (beneficiary, powerful, mobile exit, can drive amendment—low directionality, high benefit). Progressive textualist judges: d~0.50 (agenda setter, institutional power, can decline to expand, but constrained by text and amendment pressure—symmetric). Historically excluded groups: d~0.78 (victims, powerless, trapped exit, bear the cost of the supermajority requirement—high directionality, high extraction). Judicial remediation seekers: d~0.65 (payers, moderate power, constrained exit, lose a forum—high directionality). Originalists and universalists: excluded, not symmetrically positioned, but originalists lean toward higher extraction (want to freeze the clause entirely), universalists toward lower (want to expand without amendment).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic in the piton sense: there is active contention (originalists vs. universalists vs. textualists), the founding problem (scope ambiguity) remains live and contested, and the amendment mechanism is invoked when supermajority coalitions form (13th, 14th, 19th, 26th Amendments all cleared the gate successfully—the mechanism is functional, not theatrical). The constraint's persistence is not inertial theater but legitimate structural disagreement about the proper role of courts vs. democracy. However, there is a hidden mandatrophy risk: if the amendment process becomes gridlocked (polarization makes 2/3 + 3/4 supermajorities impossible for a generation), then the constraint becomes a tool for exclusion of new groups without a real path to remedy. At that point, the founding problem (scope ambiguity) would be solved by permanent exclusion (not by amendment), and the constraint would flip from tangled_rope (coordination + extraction) to snare (pure extraction). The measurement series should track amendment frequency as a proxy for gridlock: if amendments cease, mandatrophy is accelerating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_gridlock_vs_functionality,
    'Is the Article V amendment process genuinely functional as a path to scope expansion, or has polarization made supermajority consensus impossible for modern categories of rights?',
    'Historical tracking of amendment success rate and time-to-ratification for proposed civil-rights amendments post-1971 (last successful amendment period); comparative analysis of amendment outcomes in polarized vs. consensus eras.',
    'If gridlock is severe and durable, the progressive textualist reading becomes functionally equivalent to restrictive originalism (scope is frozen), and the constraint reclassifies from tangled_rope toward snare. If amendments continue to clear, the reading''s legitimacy structure remains intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gridlock_vs_functionality, empirical, 'Whether the supermajority requirement is a legitimate procedural gate or a functional exclusion mechanism due to political gridlock.').

omega_variable(
    judicial_discovery_vs_democratic_amendment,
    'Is the core distinction between this reading and expansive universalism stable, or do courts routinely read equality expansively while nominally respecting original scope (creating a hidden, un-amended expansion)?',
    'Doctrinal analysis comparing stated judicial restraint (courts declining to read expansively) against actual scope-expansion outcomes (are excluded groups gaining rights through litigation despite the textualist reading?). Track whether courts use alternative doctrines (liberty, due process, rational basis) to achieve expansion while respecting the textualist boundary on equality itself.',
    'If courts have been covertly expanding scope through alternative doctrines, the textualist reading is theatrical—the constraint''s suppression of direct expansion is real, but expansion happens anyway through doctrinal substitution. If courts genuinely respect the boundary and wait for amendment, the reading is substantive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_discovery_vs_democratic_amendment, empirical, 'Whether the textualist reading genuinely constrains expansion or merely redirects it to alternative doctrinal vehicles.').

omega_variable(
    committer_framing_ambiguity,
    'The progressive textualist reading assumes the text contains an equality principle amenable to expansion via amendment. But does the text''s meaning itself change when amended, or do amendments create new rights outside the original principle?',
    'Philosophical and historical analysis: compare how textualists describe the 13th Amendment (abolition of slavery—does equality expand to include formerly enslaved people, or does abolition create a new right outside equality?), the 14th Amendment (explicitly creates equal protection—does this expand equality, or define it anew?), and the 19th Amendment (suffrage expansion—does this extend equality to women, or create a sex-equality right?). Track whether textualists treat amendments as expansions of the original equality principle or as separate rights.',
    'If amendments are treatments as creating new rights (not expanding the original principle), then the ''bounded principle that expands via amendment'' framing is misleading—the original principle does not expand; new principles are grafted on. If amendments are genuinely expansions of a single principle, the reading''s coherence holds. The framing difference affects whether the founding problem (scope ambiguity) is solved by amendment or merely populated with new principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Whether the equality principle expands via amendment or amendments create separate new rights.').

omega_variable(
    sibling_reading_coherence,
    'Can a single interpreter hold the progressive textualist reading and the restrictive originalist reading simultaneously, or does the amendment-openness of textualism fundamentally foreclose originalism?',
    'Jurisprudential analysis: identify judges or scholars who claim both fidelity to original meaning AND amendment authority to redefine it. Assess whether this combination is logically coherent or whether it requires inconsistent commitments (original meaning is binding AND can be unbounded by amendment).',
    'If the readings coexist coherently, both originalism and textualism can serve as limits against expansive universalism. If they are in genuine tension, then the textualist reading forecloses originalism—only one can be the governing principle. This affects the expansion-resistance landscape: originalism alone vs. textualism + originalism coalition against expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coherence, conceptual, 'Whether amendment authority is logically consistent with originalist commitment to frozen meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_prog_text_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(eq_prog_text_tr_t0, projected).
narrative_ontology:measurement(eq_prog_text_tr_t10, equality_clause_scope__progressive_textualist, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(eq_prog_text_tr_t10, observed).
narrative_ontology:measurement(eq_prog_text_tr_t20, equality_clause_scope__progressive_textualist, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(eq_prog_text_tr_t20, observed).
narrative_ontology:measurement(eq_prog_text_tr_t30, equality_clause_scope__progressive_textualist, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(eq_prog_text_tr_t30, observed).
narrative_ontology:measurement(eq_prog_text_tr_t40, equality_clause_scope__progressive_textualist, theater_ratio, 40, 0.23).
narrative_ontology:measurement_basis(eq_prog_text_tr_t40, observed).
narrative_ontology:measurement(eq_prog_text_tr_t50, equality_clause_scope__progressive_textualist, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(eq_prog_text_tr_t50, projected).
narrative_ontology:measurement(eq_prog_text_tr_t60, equality_clause_scope__progressive_textualist, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(eq_prog_text_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(eq_prog_text_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(eq_prog_text_be_t0, projected).
narrative_ontology:measurement(eq_prog_text_be_t10, equality_clause_scope__progressive_textualist, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(eq_prog_text_be_t10, observed).
narrative_ontology:measurement(eq_prog_text_be_t20, equality_clause_scope__progressive_textualist, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(eq_prog_text_be_t20, observed).
narrative_ontology:measurement(eq_prog_text_be_t30, equality_clause_scope__progressive_textualist, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(eq_prog_text_be_t30, observed).
narrative_ontology:measurement(eq_prog_text_be_t40, equality_clause_scope__progressive_textualist, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(eq_prog_text_be_t40, observed).
narrative_ontology:measurement(eq_prog_text_be_t50, equality_clause_scope__progressive_textualist, base_extractiveness, 50, 0.43).
narrative_ontology:measurement_basis(eq_prog_text_be_t50, projected).
narrative_ontology:measurement(eq_prog_text_be_t60, equality_clause_scope__progressive_textualist, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(eq_prog_text_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(eq_prog_text_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(eq_prog_text_su_t0, projected).
narrative_ontology:measurement(eq_prog_text_su_t10, equality_clause_scope__progressive_textualist, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(eq_prog_text_su_t10, observed).
narrative_ontology:measurement(eq_prog_text_su_t20, equality_clause_scope__progressive_textualist, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(eq_prog_text_su_t20, observed).
narrative_ontology:measurement(eq_prog_text_su_t30, equality_clause_scope__progressive_textualist, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(eq_prog_text_su_t30, observed).
narrative_ontology:measurement(eq_prog_text_su_t40, equality_clause_scope__progressive_textualist, suppression_requirement, 40, 0.39).
narrative_ontology:measurement_basis(eq_prog_text_su_t40, observed).
narrative_ontology:measurement(eq_prog_text_su_t50, equality_clause_scope__progressive_textualist, suppression_requirement, 50, 0.39).
narrative_ontology:measurement_basis(eq_prog_text_su_t50, projected).
narrative_ontology:measurement(eq_prog_text_su_t60, equality_clause_scope__progressive_textualist, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(eq_prog_text_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, article_v_amendment_supermajority).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_review_scope__constitutional_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family: equality_clause_scope has three structural readings (restrictive_originalist, progressive_textualist, expansive_universalist), each instantiating a different constraint with different ε, beneficiary/victim structures, and extracted costs. The readings coexist in the legal system—different judges, scholars, and parties adopt different readings. Their network relationships reflect this: each reading influences and is influenced by the others through jurisprudential and political competition. The progressive textualist reading's structural delta is bounded universalism with amendment-gating; the originalist reading narrows further (no amendment authority to expand); the universalist reading widens (courts can discover universal equality). The three constraint files linked via network.affects_constraints form a constraint family, not a single constraint with measurement variance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, powerless, 0.82).
constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
