% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel_flat_control, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel_flat_control
 *   human_readable: Marriage Authority Adjudication Across Legal Pluralism
 *   domain: comparative_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   The shared commitment that some legitimate authority adjudicates marriage
 *   validity, divorce terms, inheritance rights, and child custody creates a
 *   structural constraint that operates across radically different legal
 *   systems and authority groundings. This constraint is not a single unified
 *   rule but a meta-commitment: that SOME authority (scriptural, statutory,
 *   customary, contractual) must perform this adjudication. The constraint
 *   exhibits tangled_rope structure at the flat level: it solves a genuine
 *   coordination problem (property and custody must be adjudicated across
 *   generations and communities) while simultaneously extracting from
 *   subordinated parties (typically women and children) through rules that
 *   privilege certain authority groundings and certain parties' interests.
 *   The constraint's extractiveness has increased over the 30-year interval
 *   (0.45 → 0.62) as legal pluralism has intensified — subjects caught
 *   between competing authority structures face higher costs and fewer exits.
 *   Suppression has remained high and stable (0.55 → 0.62) because the
 *   constraint operates through both structural barriers (legal disability,
 *   economic dependency, social ostracism) and internalized mechanisms
 *   (identity fusion, cultural legitimacy of authority). Theater ratio is
 *   moderate (0.48) because the authority structure performs genuine
 *   adjudication (not purely performative) but maintains substantial
 *   legitimacy theater — the invocation of authority's grounding (scriptural,
 *   statutory, customary) is partly functional (establishes precedent,
 *   coordinates expectations) and partly theatrical (maintains the
 *   institution's claim to legitimacy).
 *
 * KEY AGENTS:
 *   - Subordinated Spouses: Primary victim (powerless/trapped) — bears extraction through divorce terms, property rules, and legal disability; cannot exit without catastrophic cost
 *   - Children Without Voice: Primary victim (powerless/identity_locked) — assigned custody without consent; identity fused with parental relationship; structurally mobile but cognitively trapped
 *   - Authority Institutions: Primary beneficiary (institutional/arbitrage) — benefits from legitimacy, resource extraction (fees, tithes, social authority), and institutional continuity; can shift authority grounding without losing power
 *   - Property-Controlling Parties: Secondary beneficiary (moderate/constrained) — benefits from property rules and inheritance structures but also constrained by them; experiences mixed coordination and extraction
 *   - Legal Pluralism Subjects: Secondary victim (organized/constrained) — communities caught between competing authority structures; organized resistance (women's rights, minority communities) sees both coordination and extraction
 *   - Comparative Law Observer: Analytical observer (analytical/analytical) — sees the constraint as degraded institutional form (piton) or naturalized arrangement (false summit mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, 0.58).
domain_priors:suppression_score(marriage_authority_kernel_flat_control, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel_flat_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel_flat_control, "Marriage Authority Adjudication Across Legal Pluralism").
narrative_ontology:topic_domain(marriage_authority_kernel_flat_control, "comparative_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(marriage_authority_kernel_flat_control, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, authority_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, property_controlling_parties).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, subordinated_spouses).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, children_without_voice).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, legal_pluralism_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, property_controlling_parties).
narrative_ontology:constraint_vindicates(marriage_authority_kernel_flat_control, legitimate_authority_exists).
narrative_ontology:constraint_vindicates(marriage_authority_kernel_flat_control, marriage_is_adjudicable_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Typically women in patriarchal systems; bear extraction through divorce terms set by authority, property rules that concentrate wealth in other parties' hands, and legal disability (inability to initiate divorce, limited custody rights, restricted inheritance). Exit from marriage is legally possible but socially and economically catastrophic — loss of custody, property, social standing, and economic security. The authority structure defines the terms of exit itself, leaving no negotiating position.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, subordinated_spouses, payer,
    powerless, biographical, trapped, local).

% Assigned custody by authority adjudication without consent or voice. Identity is constituted through the parental relationship the authority assigns. Structurally mobile (could be placed with either parent, could be adopted, could age out) but cognitively trapped by identity fusion with the family relationship. The authority structure determines the child's living situation, legal status, and inheritance rights without the child's participation in the decision.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, children_without_voice, payer,
    powerless, biographical, identity_locked, local).

% Religious institutions (churches, mosques, temples), state legal systems, customary councils, or contractual arbitrators that adjudicate marriage, divorce, inheritance, and custody. Benefit from legitimacy (authority grounding), resource extraction (fees, tithes, social authority), and institutional continuity. Set the rules that govern marriage validity, divorce terms, property division, and custody assignment. Have arbitrage options — can shift authority grounding (from scriptural to statutory, from customary to contractual) without losing institutional power. The constraint sustains the institution's authority and resource flow.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, authority_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel_flat_control, authority_institutions, beneficiary).

% Often male heads of households or property owners; benefit from property rules and inheritance structures that favor their lineage and control. Also constrained by the authority structure — divorce terms may require property division, remarriage may be restricted, succession rules may bypass their preferences. Experience genuine coordination (the authority structure stabilizes property claims and inheritance) alongside extraction (the authority may enforce rules that reduce their control). Exit from marriage is possible but at significant property and status cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, property_controlling_parties, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel_flat_control, property_controlling_parties, payer).

% Communities subject to multiple overlapping authority structures (state law, religious law, customary law, contractual arrangements). Experience both coordination and extraction. The constraint coordinates property and custody across competing claims but extracts through forum-shopping, conflicting rules, and the inability to exit the entire system. Organized agents (women's rights groups, minority communities, LGBTQ+ advocates) resist the constraint through legal reform, advocacy, and alternative dispute resolution. Constrained exit — can navigate between authority structures but cannot escape the system entirely.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, legal_pluralism_subjects, payer,
    organized, generational, constrained, national).

% Scholars, judges, and policymakers who study marriage authority across legal systems. Observe that the constraint persists across radically different authority groundings (scriptural, statutory, customary, contractual) and that the actual function — stabilizing property and custody — could be performed by contract, negotiation, or neutral arbitration without invoking authority. See the constraint as a degraded institutional form (piton) where the authority structure maintains its claim to adjudicate not because adjudication requires authority, but because the institution's survival depends on the claim. Risk naturalizing contingent arrangements as inevitable features of social organization.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel_flat_control, comparative_law_observers, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(marriage_authority_kernel_flat_control, comparative_law_observers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adjudicate marriage validity, divorce terms, property division, inheritance rights, and child custody across generations and communities. Stabilize expectations about who is married, who owns what, and who has parental authority. Enable property transfer and inheritance succession. Provide dispute resolution when parties disagree about marriage status, divorce terms, or custody.
% TRANSFER_FUNCTION: The constraint moves property (from one spouse to another upon divorce, from deceased to heirs upon death), parental authority (from one parent to another upon custody assignment), and social status (marriage validity determines legal rights and social standing). The direction of transfer is determined by the authority structure's rules: in patriarchal systems, property and custody typically flow toward male heads of households; in egalitarian systems, property and custody are divided more equally. The constraint also transfers legitimacy and authority from the authority institution to the parties it adjudicates.
% ABSENT_VOICES: Children have no voice in custody assignment despite bearing the consequences. Subordinated spouses (typically women) have limited voice in divorce terms and property division despite bearing extraction. Minority communities subject to plural authority structures have limited voice in which authority structure governs their marriage and property. LGBTQ+ individuals have been historically excluded from marriage adjudication entirely, and in many jurisdictions remain excluded or subordinated. These absent voices would object to rules that subordinate them, restrict their exit options, or deny them voice in decisions that affect their lives.
% DISAPPEARANCE_RATIONALE: If the marriage authority constraint disappeared overnight, the world would partially rearrange and partially remain unchanged. Property and custody would need to be adjudicated through some mechanism (contract, negotiation, neutral arbitration, or alternative authority structures). Some communities would develop alternative dispute resolution mechanisms; others would revert to customary or family-based adjudication. The coordination function would persist but the extraction mechanism would change — subordinated parties might gain voice and exit options if authority institutions lost their monopoly on adjudication. However, some form of authority adjudication would likely re-emerge because property and custody require coordination across generations and communities. The constraint is not inevitable, but the coordination problem it solves is real.
% FOUNDING_PROBLEM: The founding problem is the need to adjudicate marriage validity, divorce terms, property division, inheritance rights, and child custody in societies with multiple parties, generations, and competing claims. Without some mechanism for adjudication, property disputes would be endless, inheritance would be contested, and custody would be determined by force rather than law. The constraint emerged to solve this coordination problem by vesting adjudication power in authority institutions (religious, state, customary, contractual) that could establish precedent, coordinate expectations, and enforce decisions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live in all contemporary societies — property and custody must be adjudicated, and disputes arise regularly. However, the corroboration is contested. Authority institutions (churches, state legal systems, customary councils) attest that the founding problem requires their adjudication power. Alternative dispute resolution advocates (mediators, arbitrators, contractual parties) attest that the founding problem can be solved through negotiation and contract without invoking authority. Subordinated parties attest that the founding problem is being solved at their expense — the authority structure adjudicates in ways that extract from them rather than in ways that serve the coordination function. The corroboration is split: authority institutions and property-controlling parties attest to the necessity of authority adjudication; subordinated parties and legal pluralism subjects attest to the possibility of alternative mechanisms.
narrative_ontology:disappearance_verdict(marriage_authority_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(marriage_authority_kernel_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED SPOUSE (SNARE) — Trapped by economic dependency, social ostracism, and legal disability. Cannot exit marriage without losing custody, property, or social standing. The authority structure enforces rules that extract from this agent: divorce terms are set by the authority, not negotiated; property rights are defined by the authority's reading of tradition or statute; exit is legally possible but socially/economically catastrophic. Maximum experienced extraction — the constraint defines the terms of exit itself.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CHILD WITHOUT VOICE (SNARE) — Structurally mobile (could be placed with either parent, could be adopted, could age out) but identity-locked into the parental relationship. Custody is determined by the authority structure; the child has no legal standing to contest it. The constraint extracts from the child through assignment to a custodian without consent. Identity lock is cognitive/relational: the child's identity is constituted through the family relationship the authority adjudicates. Exit would require becoming a different person — abandoning the identity the constraint assigns.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: PROPERTY-CONTROLLING PARTY (TANGLED ROPE) — Often benefits from the authority structure (property rights defined in their favor, inheritance rules favor their lineage) but also constrained by it (divorce terms may require property division, remarriage may be restricted, succession rules may bypass their preferences). Experiences genuine coordination (the authority structure stabilizes property claims and inheritance) alongside extraction (the authority may enforce rules that reduce their control). Constrained exit: could exit the marriage but at significant property/status cost.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: AUTHORITY INSTITUTION (ROPE) — Benefits from the constraint through legitimacy, resource extraction (fees, tithes, social authority), and institutional continuity. Experiences the constraint as coordination: adjudicating marriage, divorce, inheritance, and custody is the institution's core function. The institution has arbitrage options — it can shift its authority grounding (from scriptural to statutory, from customary to contractual) without losing institutional power. Net beneficiary — the constraint sustains the institution's authority and resource flow.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGAL PLURALISM SUBJECT (TANGLED ROPE) — Communities subject to multiple overlapping authority structures (state law, religious law, customary law, contractual arrangements) experience both coordination and extraction. The constraint coordinates property and custody across competing claims but extracts through forum-shopping, conflicting rules, and the inability to exit the entire system. Organized agents (women's rights groups, minority communities, LGBTQ+ advocates) see the constraint as a hybrid: genuine coordination function (someone must adjudicate marriage validity) alongside systematic extraction (the authority structure privileges certain communities and rules over others).
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPARATIVE LAW OBSERVER (PITON) — From a civilizational perspective, the constraint appears as a degraded institutional form: the claim that 'legitimate authority adjudicates marriage' persists across radically different authority groundings (scriptural, statutory, customary, contractual), but the actual function — stabilizing property and custody — could be performed by contract, negotiation, or neutral arbitration without invoking authority at all. The theater is the invocation of legitimacy itself: the authority structure maintains its claim to adjudicate not because adjudication requires authority, but because the institution's survival depends on the claim. Theater ratio reflects that much of the authority's work is performative legitimacy-maintenance rather than functional adjudication.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/analytical perspective, some authority structure for marriage adjudication appears inevitable: property, inheritance, and custody require coordination across generations and communities. The constraint appears as a natural law — any stable society must have some mechanism for adjudicating marriage validity and its consequences. However, the structural data contradicts this: the authority structure is contingent (grounding differs radically), beneficiaries are identifiable (authority institutions, property-controlling parties), and suppression is substantial (trapped and identity-locked agents). The engine will compute this as a false summit, revealing that 'inevitable authority' naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(marriage_authority_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint solves a genuine coordination problem (property and custody must be adjudicated) but extracts substantially from subordinated parties through rules that privilege certain authority groundings and certain parties' interests. The extraction is not maximal because some parties (authority institutions, property-controlling parties) genuinely benefit from coordination, and some subordinated parties have constrained (not trapped) exit options. The 30-year trajectory shows increasing extractiveness (0.45 → 0.62) as legal pluralism has intensified — subjects caught between competing authority structures face higher costs. Suppression (0.62): Moderate-high and stable. The constraint operates through both structural barriers (legal disability, economic dependency, social ostracism) and internalized mechanisms (identity fusion, cultural legitimacy of authority). Structural suppression includes legal disability (women's inability to initiate divorce in some systems), economic dependency (property rules that concentrate wealth), and social ostracism (divorce stigma). Internalized suppression includes identity fusion (children's identity constituted through parental relationship) and cultural legitimacy (acceptance of authority's grounding as natural or divinely ordained). Theater ratio (0.48): Moderate. The authority structure performs genuine adjudication (not purely performative) but maintains substantial legitimacy theater. The invocation of authority's grounding (scriptural, statutory, customary) is partly functional (establishes precedent, coordinates expectations) and partly theatrical (maintains the institution's claim to legitimacy). Theater is lower than in purely performative constraints because adjudication has real consequences (property is transferred, custody is assigned) and real coordination function (expectations are stabilized). Theater is not negligible because much of the authority's work is maintaining the claim to legitimacy rather than performing adjudication that could not be done through contract or negotiation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from the same structural data. The subordinated spouse sees snare (trapped by legal disability and economic dependency, extraction is maximal). The child sees snare (identity-locked into parental relationship, no voice in custody assignment). The property-controlling party sees tangled_rope (genuine coordination of property and inheritance alongside extraction through rules that may reduce their control). The authority institution sees rope (pure coordination, the constraint is the institution's core function). The legal pluralism subject sees tangled_rope (coordination through pluralism alongside extraction through conflicting rules). The comparative law observer sees piton (degraded institutional form, theater is substantial). The analytical observer risks seeing mountain (authority adjudication is inevitable) but the structural data reveals this as a false summit: the authority structure is contingent (grounding differs radically), beneficiaries are identifiable (authority institutions, property-controlling parties), and suppression is substantial (trapped and identity-locked agents). The perspectival gap reveals that the constraint's type depends entirely on the observer's structural position: beneficiaries see coordination, victims see extraction, organized agents see mixed coordination-extraction, and analytical observers risk naturalizing contingent arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. Subordinated spouses and children are full targets (d ≈ 1.0): they bear extraction through rules that privilege other parties' interests and have minimal exit options. Property-controlling parties are partial targets (d ≈ 0.6): they benefit from some rules (property, inheritance) but are constrained by others (divorce terms, remarriage restrictions). Authority institutions are full beneficiaries (d ≈ 0.0): they benefit from legitimacy, resource extraction, and institutional continuity, and have arbitrage options (can shift authority grounding). Legal pluralism subjects are partial targets (d ≈ 0.65): they experience extraction through conflicting rules and forum-shopping costs, but organized agents have some agency and exit options. The engine derives d from beneficiary/victim declarations and exit options: victims with trapped or identity_locked exit get high d; beneficiaries with arbitrage exit get low d; constrained agents get intermediate d. Effective extraction (χ) is then computed from d and scope: larger scope (regional, national) amplifies effective extraction for targets and damps it for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's mandate is 'adjudicate marriage validity, divorce terms, inheritance rights, and child custody.' This mandate is live and functional — the constraint performs genuine adjudication that coordinates property and custody across generations and communities. However, the constraint exhibits extraction alongside coordination, which is the defining feature of tangled_rope. The mandatrophy is resolved by recognizing that the constraint's function (coordination) is genuine but its distribution (extraction from subordinated parties) is asymmetric. The constraint does not suffer from mandate obsolescence (the function is still needed) but from mandate capture (the authority structure uses its adjudication power to extract from subordinated parties). The false summit perspective (mountain / natural law) is the key diagnostic: the claim that 'legitimate authority must adjudicate marriage' naturalizes what is actually a contingent institutional arrangement that benefits authority institutions and property-controlling parties. The constraint is not inevitable; it is a choice to vest adjudication power in authority institutions rather than in contract, negotiation, or neutral arbitration. This choice is defended through legitimacy theater (invocation of scriptural, statutory, customary, or contractual grounding) rather than through functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_grounding_contingency,
    'Is the grounding of marriage authority (scriptural, statutory, customary, contractual) a fundamental feature of the constraint, or a contingent historical artifact?',
    'Comparative historical analysis: do societies with radically different authority groundings produce equivalent marriage stability, property protection, and custody outcomes? Or do the groundings themselves determine the distribution of extraction?',
    'If grounding is fundamental: the constraint is a mountain (any authority structure works). If grounding is contingent: the constraint is tangled_rope (the choice of grounding determines who benefits and who pays).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_contingency, empirical, 'Whether authority grounding is fundamental or contingent').

omega_variable(
    coordination_necessity,
    'Does marriage adjudication require legitimate authority, or could contract, negotiation, and neutral arbitration perform the same coordination function without invoking authority?',
    'Comparative analysis of jurisdictions with minimal authority involvement (contractual marriage, arbitration-based divorce, parental agreement on custody) vs. authority-adjudicated systems. Measure stability, dispute resolution, and property protection outcomes.',
    'If authority is necessary: the constraint is rope (pure coordination). If authority is contingent: the constraint is snare (authority is cover for extraction). If both are possible: the constraint is tangled_rope (genuine coordination alongside extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity, empirical, 'Whether legitimate authority is necessary for marriage coordination').

omega_variable(
    subordination_mechanism_structural_vs_cultural,
    'Is the subordination of certain spouses (typically women) a structural feature of the authority constraint itself, or a cultural artifact that could be reformed within the same authority structure?',
    'Comparative analysis of authority structures with identical grounding (e.g., Islamic law, statutory law) but different substantive rules regarding spousal rights. If identical groundings produce different subordination outcomes, the mechanism is cultural/reformable. If identical groundings produce identical subordination, the mechanism is structural.',
    'If structural: the constraint is snare (subordination is built into the authority structure). If cultural: the constraint is tangled_rope (coordination function is genuine, but current rules extract from subordinated parties). If reformable: the constraint is scaffold (temporary subordination being phased out).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_mechanism_structural_vs_cultural, empirical, 'Whether subordination is structural or cultural within authority systems').

omega_variable(
    legal_pluralism_extraction_mechanism,
    'Does legal pluralism (multiple overlapping authority structures) increase or decrease extraction from subjects caught between competing claims?',
    'Comparative analysis of jurisdictions with unified authority (one grounding, one set of rules) vs. plural authority (multiple groundings, conflicting rules). Measure dispute resolution costs, forum-shopping, and outcomes for vulnerable parties.',
    'If pluralism increases extraction: the constraint is snare (subjects are trapped between competing authorities). If pluralism decreases extraction: the constraint is rope (pluralism enables exit through forum choice). If pluralism has mixed effects: the constraint is tangled_rope (coordination through pluralism alongside extraction through conflict).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_pluralism_extraction_mechanism, empirical, 'Whether legal pluralism increases or decreases extraction').

omega_variable(
    false_summit_natural_authority,
    'Is the claim that ''legitimate authority must adjudicate marriage'' a natural law or a naturalized institutional arrangement?',
    'Historical analysis: do societies without centralized authority (stateless societies, anarchist communities, contractual networks) successfully coordinate marriage, property, and custody without invoking authority? If yes, the claim is naturalized, not natural.',
    'If natural law: the constraint is mountain (inevitable). If naturalized: the constraint is tangled_rope or snare (contingent institutional arrangement that benefits authority institutions and property-controlling parties).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_authority, conceptual, 'Whether authority adjudication is natural law or naturalized arrangement').

omega_variable(
    identity_lock_persistence_post_exit,
    'For children assigned custody by authority adjudication, does the identity lock persist after exit (aging out, emancipation, or parental death)?',
    'Longitudinal study: do adults who were assigned custody as children show persistent identity fusion with the parental relationship, or does the lock dissolve upon exit? If persistent, the lock is internalized; if dissolved, the lock was structural.',
    'If persistent: the constraint''s suppression is higher than measured (internalized lock carries extraction beyond exit). If dissolved: the constraint''s suppression is accurately measured (structural lock only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_exit, empirical, 'Whether identity lock persists after exit from custody assignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_auth_tr_t0, marriage_authority_kernel_flat_control, theater_ratio, 0, 0.42).
narrative_ontology:measurement(marr_auth_tr_t10, marriage_authority_kernel_flat_control, theater_ratio, 10, 0.45).
narrative_ontology:measurement(marr_auth_tr_t20, marriage_authority_kernel_flat_control, theater_ratio, 20, 0.48).
narrative_ontology:measurement(marr_auth_tr_t30, marriage_authority_kernel_flat_control, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(marr_auth_be_t0, marriage_authority_kernel_flat_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_auth_be_t10, marriage_authority_kernel_flat_control, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(marr_auth_be_t20, marriage_authority_kernel_flat_control, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_auth_be_t30, marriage_authority_kernel_flat_control, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_auth_su_t0, marriage_authority_kernel_flat_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_auth_su_t10, marriage_authority_kernel_flat_control, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(marr_auth_su_t20, marriage_authority_kernel_flat_control, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(marr_auth_su_t30, marriage_authority_kernel_flat_control, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel_flat_control, resource_allocation).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, spousal_property_rights).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, child_custody_assignment).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, inheritance_succession).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, divorce_exit_costs).

% DUAL FORMULATION NOTE:
% The marriage authority constraint is upstream of specific property, custody, and inheritance constraints. Each downstream constraint has its own extractiveness value reflecting the specific rules the authority enforces; the marriage authority constraint has its own extractiveness reflecting the asymmetry of adjudication power itself. The network captures how the authority structure's choice of grounding (scriptural, statutory, customary, contractual) cascades into specific extraction mechanisms in property, custody, and inheritance domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel_flat_control, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
