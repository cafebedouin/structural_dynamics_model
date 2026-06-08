% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel_flat_control, []).

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
 *   constraint_id: catastrophe_memory_kernel_flat_control
 *   human_readable: Ritual Catastrophe-Commemoration as Collective Memory Transmission Across Diaspora
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual catastrophe-commemoration represents a structural mechanism
 *   through which diaspora communities transmit collective memory of
 *   catastrophic historical events across millennia of dispersal,
 *   generational turnover, and cultural pressure toward assimilation. The
 *   constraint operates at the intersection of genuine coordination
 *   (maintaining shared identity and historical consciousness) and
 *   institutional extraction (maintaining religious authority and resource
 *   flows). The same ritual structure that solves the real problem of memory
 *   persistence across diaspora also enforces conformity, suppresses
 *   alternative narratives, and binds participants through identity-fusion
 *   rather than voluntary participation. The theater ratio (0.58) reflects
 *   that much of the ritual apparatus is performative: recitation of
 *   narratives, symbolic action, and prescribed participation persist largely
 *   through institutional maintenance rather than through functional
 *   effectiveness at transmitting accurate historical knowledge. Over the
 *   1000-year interval, theater ratio has increased from 0.35 to 0.62,
 *   indicating that the performative content has grown relative to the
 *   functional content — the ritual increasingly maintains institutional
 *   authority and community identity rather than serving as a mechanism for
 *   historical learning. Simultaneously, base extractiveness has risen from
 *   0.28 to 0.38, and suppression requirement from 0.40 to 0.50, suggesting
 *   that maintaining the constraint requires increasing enforcement effort as
 *   alternative transmission mechanisms (written history, secular education,
 *   digital archives) become available and as diaspora communities develop
 *   heterodox interpretations of the catastrophe narrative.
 *
 * KEY AGENTS:
 *   - Diaspora Community Members: Primary victims (powerless/identity_locked) — bear extraction through mandatory participation, emotional labor, and identity fusion with the ritual
 *   - Diaspora Community (Collective): Secondary actor (moderate/constrained) — benefits from shared identity and historical continuity; constrained by conformity pressure and interpretive closure
 *   - Religious Institution: Primary beneficiary (institutional/arbitrage) — benefits from ritual participation (legitimacy, authority, resource flows); has high exit optionality and can modify rituals
 *   - Assimilationist Members: Secondary victim (powerful/mobile) — structurally mobile but experience extraction through social pressure and identity conflict
 *   - Ritual Performance Apparatus: Institutional actor (institutional/arbitrage) — maintains performative structure; benefits from institutional authority; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of human memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel_flat_control, 0.35).
domain_priors:suppression_score(catastrophe_memory_kernel_flat_control, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel_flat_control, "Ritual Catastrophe-Commemoration as Collective Memory Transmission Across Diaspora").
narrative_ontology:topic_domain(catastrophe_memory_kernel_flat_control, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_memory_kernel_flat_control, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, diaspora_community_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, religious_institutional_authority).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, individual_agency_in_ritual_participation).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, historical_accuracy_contestation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, diaspora_community_collective).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, religious_institution).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, diaspora_member_identity_locked).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, diaspora_community_collective).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, assimilationist_member).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel_flat_control, collective_memory_requires_ritual_reinforcement).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel_flat_control, diaspora_identity_depends_on_shared_commemoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in ritual commemoration because their identity as a diaspora member is constituted through the ritual. Skipping the ritual would require abandoning the identity itself. Bears the cost of mandatory participation, emotional labor, and conformity pressure. Cannot exit without becoming a different person.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, diaspora_member_identity_locked, payer,
    powerless, biographical, identity_locked, global).

% Benefits from shared identity and historical continuity maintained through ritual. Simultaneously bears the cost of conformity pressure and interpretive closure. Can exit the community but at high relational cost. The constraint both enables and constrains the community's collective action.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, diaspora_community_collective, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel_flat_control, diaspora_community_collective, payer).

% Sets and administers the ritual structure. Benefits from ritual participation through legitimacy, cultural authority, and resource flows. Can modify rituals, reinterpret doctrine, and adapt to new contexts. Subsidizes ritual infrastructure as investment in institutional continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, religious_institution, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel_flat_control, religious_institution, beneficiary).

% Structurally mobile: can exit rituals, assimilate into host culture, redefine identity. But experiences extraction through social pressure (family disapproval, community sanctions) and identity conflict (tension between diaspora identity and assimilationist goals). Bears the cost of dissent or reinterpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, assimilationist_member, payer,
    powerful, generational, mobile, global).

% The formal structure of commemoration (liturgy, calendar, prescribed narratives) that persists through institutional maintenance. Sees its own function as degraded — the original purpose (transmitting accurate historical memory) has partially atrophied, and the apparatus now functions primarily to perform identity and maintain institutional authority. Maintained through inertia rather than functional necessity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, ritual_performance_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Not an agent but a non-agent entity: the abstract good of historical accuracy in catastrophe narratives. Excluded from the ritual decision-making process. Bears the cost of narrative ossification and distortion as rituals prioritize institutional authority and identity performance over historical precision.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, historical_accuracy, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel_flat_control, historical_accuracy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining collective identity and historical consciousness of diaspora communities across dispersal, generational turnover, and cultural pressure toward assimilation. The real coordination problem: how do scattered communities separated by geography and time maintain shared memory of catastrophic historical events that define their identity?
% TRANSFER_FUNCTION: The constraint transfers emotional labor, conformity cost, and interpretive closure FROM diaspora members TO the religious institution and the diaspora community collective. It transfers legitimacy, cultural authority, and resource flows FROM diaspora members TO the religious institution. It transfers historical narrative control FROM alternative sources (written history, secular education) TO the ritual apparatus.
% ABSENT_VOICES: Historical actors who experienced the original catastrophe but are now deceased; diaspora members who have assimilated and exited the community; scholars and historians who contest the ritual narratives; secular diaspora communities that have abandoned rituals; alternative transmission mechanisms (written archives, digital history) that are not represented in ritual decision-making.
% DISAPPEARANCE_RATIONALE: If ritual catastrophe-commemoration disappeared overnight, diaspora communities would need to develop alternative mechanisms for maintaining collective identity and historical consciousness. Some communities would develop written/secular alternatives (historical societies, educational programs, digital archives). Some would assimilate more rapidly into host cultures. Some would develop new ritual forms. The religious institution would lose legitimacy and resource flows. The constraint's disappearance would not leave the world unchanged — it would force reorganization of how diaspora identity is maintained and transmitted.
% FOUNDING_PROBLEM: The founding problem was the practical necessity of transmitting catastrophe narratives and diaspora identity across generations and geographic dispersal in pre-literate or low-literacy contexts where written records were fragile, inaccessible, or controlled by hostile authorities. Ritual provided a mechanism for embedding historical memory in repeated action, emotional resonance, and community participation — making the narrative resistant to forgetting and to external suppression.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists of diaspora communities attest that ritual commemoration was historically necessary for memory transmission in contexts of dispersal and literacy barriers. However, the necessity of ritual is contested: some scholars argue that written history, oral tradition, and secular education can transmit memory equally effectively; others argue that ritual provides unique emotional and identity-binding functions that secular mechanisms cannot replicate. The religious institution attests that ritual remains necessary for maintaining diaspora identity; assimilationist members and secular scholars contest this claim.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA MEMBER (SNARE) — Structurally mobile (could skip rituals, relocate, assimilate) but identity-fused with the commemoration practice. The ritual is constitutive of their identity as a diaspora member; exit would require abandoning the identity itself. Experiences maximum extraction: mandatory participation, emotional labor, time cost, and the constraint that forgetting is identity death. The identity lock is the binding mechanism, not material barriers.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIASPORA COMMUNITY (TANGLED ROPE) — Genuine coordination function: rituals solve the real problem of maintaining collective identity and historical consciousness across dispersed populations and generational time. Simultaneous extraction: ritual participation is mandatory for community standing; dissent or reinterpretation triggers social penalty. Benefits from the constraint (shared identity, historical continuity) and bears costs (conformity pressure, interpretive closure). Constrained exit: leaving the community is possible but carries high social and relational cost.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTION (ROPE) — Experiences the constraint as pure coordination: rituals solve the institutional problem of transmitting doctrine, maintaining authority, and ensuring continuity across diaspora. The institution benefits from ritual participation (legitimacy, resource flows, cultural authority) and has high exit optionality (can modify rituals, reinterpret doctrine, adapt to new contexts). Effective extraction is low or negative — the institution subsidizes the coordination mechanism through resource investment in ritual infrastructure.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASSIMILATIONIST MEMBER (TANGLED ROPE) — Structurally mobile (can exit rituals, assimilate into host culture, redefine identity) but experiences extraction through social pressure and identity conflict. The constraint coordinates community identity but extracts conformity cost from those who wish to redefine their relationship to the catastrophe narrative. Benefits from community belonging if they participate; bears extraction cost if they dissent. Mobile exit means the extraction is not maximal, but the constraint still requires active enforcement (social sanctions, family pressure, institutional disapproval).
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RITUAL PERFORMANCE APPARATUS (PITON) — The formal structure of commemoration (liturgy, calendar, prescribed narratives) persists largely through institutional inertia and theatrical maintenance. The original function — transmitting accurate historical memory — has partially atrophied; the ritual now functions primarily to perform identity and maintain institutional authority. Theater ratio is high (0.58): much of the ritual activity is performative (recitation, symbolic action) rather than functional (genuine historical learning, contestation of narrative). The apparatus is maintained because alternatives haven't fully replaced it and because the institution benefits from the performance, not because the ritual mechanism is optimally effective.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of ritual commemoration appears as an immutable feature of human collective memory: all diaspora communities across history have developed ritual mechanisms to transmit catastrophe narratives. The constraint appears as a natural law of how collective memory persists across time and space. However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The beneficiary declarations and the extraction metrics suggest this is a false summit: the constraint benefits identifiable institutional actors and requires active enforcement, indicating it is constructed rather than natural.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts conformity and emotional labor from diaspora members, but the extraction is not maximal because the coordination function is genuine — rituals do solve the real problem of maintaining collective identity across diaspora. The beneficiary (religious institution) subsidizes the coordination mechanism through resource investment, which reduces effective extraction. The value reflects that the constraint is hybrid: genuine coordination with embedded extraction, not pure extraction. Suppression (0.48): Moderate. Barriers to exit include social cost (family pressure, community disapproval), relational dependency (identity fusion), and institutional enforcement (ritual requirements, doctrinal authority). But suppression is not total — some members do exit (assimilationists), and alternative transmission mechanisms exist. Theater ratio (0.58): Moderate-high. The ritual apparatus is substantially performative: much of the activity is recitation, symbolic action, and prescribed participation rather than genuine historical learning or contestation of narrative. The theater has increased over the interval as the original function (transmitting accurate historical memory) has partially atrophied and the ritual has become primarily a mechanism for maintaining institutional authority and community identity. The rise in theater ratio over 1000 years reflects Goodhart drift: the ritual's original purpose (memory transmission) has been replaced by proxy goals (institutional authority, identity performance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival disagreement across structural positions. The diaspora member (powerless/identity_locked) sees a snare: mandatory participation, identity fusion, and extraction with no exit. The diaspora community (moderate/constrained) sees tangled rope: genuine coordination of identity and memory alongside conformity pressure. The religious institution (institutional/arbitrage) sees rope: pure coordination with no experienced extraction. The assimilationist member (powerful/mobile) sees tangled rope: structural mobility but extraction through social pressure. The ritual apparatus (institutional/arbitrage) sees itself as piton: degraded function maintained through inertia. The analytical observer risks seeing mountain: ritual commemoration as a natural law of human collective memory. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural position — their power, exit options, and relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Diaspora members are victims with identity-locked exit: high d → high experienced extraction (snare). The diaspora community benefits from coordination but bears conformity cost: moderate d → moderate experienced extraction (tangled rope). The religious institution is a beneficiary with arbitrage exit: low d → low/negative experienced extraction (rope). Assimilationist members are victims with mobile exit: moderate d → moderate experienced extraction (tangled rope). The ritual apparatus is a beneficiary with arbitrage exit: low d → low experienced extraction (piton). The analytical observer has analytical exit: d is not computed, classification is independent of directionality. The engine derives d from beneficiary/victim declarations and exit modulation; the perspectival gap emerges from how different agents experience the same constraint through different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to transmit collective memory of catastrophe across diaspora. Over 1000 years, this mandate has partially outlived its function: alternative transmission mechanisms (written history, secular education, digital archives) now exist and are increasingly effective. The ritual persists not because it is the only mechanism for memory transmission, but because it maintains institutional authority and community identity. The rising theater ratio (0.35 → 0.62) and rising suppression requirement (0.40 → 0.50) indicate that maintaining the constraint requires increasing enforcement effort as the original function becomes less necessary. The constraint exhibits mandatrophy: the original purpose (memory transmission) has been replaced by proxy goals (institutional authority, identity performance), and the constraint persists through institutional inertia rather than functional necessity. However, the constraint is not purely mandatrophic — the coordination function (maintaining diaspora identity) remains genuine and valuable. The mandatrophy is partial: the constraint solves a real coordination problem (diaspora identity) while simultaneously maintaining institutional authority through performative ritual. The classification as tangled rope (not piton) reflects this hybrid: genuine coordination with embedded extraction, not pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is ritual catastrophe-commemoration a natural law of collective memory transmission, or a constructed institutional arrangement that benefits specific actors?',
    'Comparative analysis of diaspora communities with and without formalized ritual commemoration; examination of whether communities that abandon rituals lose historical memory or develop alternative transmission mechanisms; historical analysis of how rituals originated and evolved',
    'If natural law: mountain classification confirmed; beneficiaries are incidental. If constructed: false summit confirmed; the constraint is tangled_rope or snare depending on extraction severity and enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Whether ritual commemoration is natural or constructed').

omega_variable(
    identity_lock_mechanism,
    'Is the binding mechanism for diaspora members primarily identity-fusion (cognitive/psychological) or material barriers (social cost, relational dependency)?',
    'Ethnographic study of members who exit rituals: do they report identity dissolution or material penalty? Analysis of whether members with low social cost to exit (e.g., geographically isolated, economically independent) still participate; comparison of exit rates across different diaspora contexts with varying social cost structures',
    'If primarily identity-locked: the constraint is a snare with cognitive binding; exit is psychologically impossible despite structural mobility. If primarily material: the constraint is tangled_rope with social/relational barriers; exit is costly but structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether binding is identity-fusion or material barriers').

omega_variable(
    historical_accuracy_preservation,
    'Do formalized rituals actually preserve historical accuracy of catastrophe narratives, or do they ossify and distort narratives over time?',
    'Comparison of ritual-transmitted narratives with historical documentation; analysis of how narratives change across ritual iterations; study of whether ritual communities develop alternative narratives (heterodox interpretations) and how institutions respond',
    'If rituals preserve accuracy: the coordination function is genuine and the constraint is primarily rope. If rituals distort: the constraint is primarily extractive (snare) or performative (piton); the ''memory transmission'' is cover story for identity maintenance and institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_accuracy_preservation, empirical, 'Whether rituals preserve or distort historical narratives').

omega_variable(
    alternative_transmission_mechanisms,
    'Do non-ritual mechanisms (written history, secular education, digital archives, diaspora literature) transmit catastrophe memory as effectively as rituals?',
    'Comparative study of historical knowledge retention in diaspora communities with strong rituals vs. those relying on secular transmission; analysis of whether younger generations in ritual communities have better historical knowledge than those in non-ritual communities; examination of whether ritual communities develop written/secular alternatives',
    'If rituals are uniquely effective: the constraint is rope (genuine coordination). If alternatives are equally effective: the constraint is primarily extractive (snare) or performative (piton); rituals persist due to institutional benefit, not functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transmission_mechanisms, empirical, 'Whether rituals are uniquely effective for memory transmission').

omega_variable(
    institutional_authority_extraction,
    'How much of the institutional benefit from rituals derives from genuine coordination (maintaining diaspora identity) versus extraction (maintaining institutional authority and resource flows)?',
    'Analysis of how institutions respond to ritual reinterpretation or modification; examination of whether institutions prioritize historical accuracy or narrative control; study of how institutions allocate resources (ritual infrastructure vs. historical research); comparison of institutional behavior when rituals are questioned',
    'If primarily coordination: the constraint is rope from the institutional perspective. If primarily extraction: the constraint is snare or tangled_rope; the institution uses ritual coordination as cover for authority maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_extraction, empirical, 'Institutional benefit from coordination versus authority extraction').

omega_variable(
    diaspora_identity_dependency,
    'Is diaspora identity genuinely dependent on ritual participation, or is the dependency constructed through institutional messaging and social enforcement?',
    'Study of diaspora members who maintain identity without ritual participation; analysis of how identity is constructed in diaspora communities with weak ritual traditions; examination of whether identity strengthens or weakens when rituals are questioned or modified; historical analysis of how diaspora identity evolved before formalized rituals',
    'If genuinely dependent: the constraint is rope (coordination solves a real problem). If constructed: the constraint is snare (extraction mechanism disguised as identity necessity); the institution manufactures dependency to maintain control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_identity_dependency, conceptual, 'Whether diaspora identity is genuinely dependent on rituals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel_flat_control, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_tr_t0, catastrophe_memory_kernel_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_tr_t100, catastrophe_memory_kernel_flat_control, theater_ratio, 100, 0.45).
narrative_ontology:measurement(catmem_tr_t500, catastrophe_memory_kernel_flat_control, theater_ratio, 500, 0.58).
narrative_ontology:measurement(catmem_tr_t1000, catastrophe_memory_kernel_flat_control, theater_ratio, 1000, 0.62).

% Extraction over time
narrative_ontology:measurement(catmem_be_t0, catastrophe_memory_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(catmem_be_t100, catastrophe_memory_kernel_flat_control, base_extractiveness, 100, 0.32).
narrative_ontology:measurement(catmem_be_t500, catastrophe_memory_kernel_flat_control, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(catmem_be_t1000, catastrophe_memory_kernel_flat_control, base_extractiveness, 1000, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catmem_su_t0, catastrophe_memory_kernel_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(catmem_su_t100, catastrophe_memory_kernel_flat_control, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(catmem_su_t500, catastrophe_memory_kernel_flat_control, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(catmem_su_t1000, catastrophe_memory_kernel_flat_control, suppression_requirement, 1000, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel_flat_control, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, diaspora_assimilation_pressure).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, religious_institutional_authority).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, historical_narrative_contestation).

% DUAL FORMULATION NOTE:
% Ritual catastrophe-commemoration is a constraint family with multiple structurally distinct components: (1) the coordination function (maintaining diaspora identity across dispersal), (2) the institutional extraction (maintaining religious authority), and (3) the performative apparatus (ritual maintenance through inertia). These could be decomposed into separate stories with different ε values, but the flat construction treats them as a single hybrid constraint (tangled rope) because they are structurally inseparable in practice — the coordination and extraction mechanisms operate through the same ritual structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
