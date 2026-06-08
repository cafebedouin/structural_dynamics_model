% ============================================================================
% CONSTRAINT STORY: constitutional_federalism_tension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_federalism_tension, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_federalism_tension
 *   human_readable: India's Parallel Personal Law System: Constitutional Federalism Tension
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   India's parallel personal law system emerged from the 1947 Partition
 *   settlement and the framers' compromise between secular constitutional
 *   principles and religious autonomy. Hindu, Muslim, Christian, Parsi, and
 *   secular civil frameworks govern marriage, divorce, inheritance, adoption,
 *   and custody simultaneously within one state, creating a complex
 *   jurisdictional lattice where a citizen's legal rights depend on their
 *   declared religious identity. The system has persisted for 75 years
 *   without zero-sum displacement, weathering periodic reform attempts and
 *   communal crises. The 1985 Shah Bano case exemplifies the tension: the
 *   Supreme Court awarded maintenance to a divorced Muslim woman under
 *   secular criminal law, Islamic authorities protested the intrusion into
 *   Sharia, and Parliament reversed the ruling to preserve religious autonomy
 *   — a sequence that revealed the system's extractive dimension (women lost
 *   constitutional protections) while confirming its coordination function
 *   (prevented communal conflict). The constraint exhibits all six DR types
 *   from different structural positions, demonstrating how the same
 *   institutional arrangement can simultaneously coordinate religious
 *   pluralism and extract from identity-locked populations.
 *
 * KEY AGENTS:
 *   - Women in traditional religious communities: Primary victims (powerless/identity_locked) — bear gender-unequal provisions, cannot exit without abandoning community and self-concept
 *   - Interfaith couples: Secondary victims (moderate/constrained) — forced choice between religious frameworks or stigmatized civil marriage; coordination exists but extracts through forced rejection of one tradition
 *   - Religious institutional authorities: Primary beneficiaries (institutional/arbitrage) — preserve jurisdictional power over family law; can arbitrage between religious and secular frameworks
 *   - Secular liberal elites: Secondary beneficiaries (institutional/arbitrage) — access civil framework without penalty; use system's existence as pluralist credential
 *   - Religious minorities within communities: Mixed position (moderate/constrained) — benefit from tradition's autonomy against Hindu majoritarianism; bear extraction from internal hierarchy
 *   - Uniform Civil Code movement: Organized coalition (organized/mobile) — sees system as temporary; sunset logic contested (reform vs assimilation)
 *   - Constitutional courts: Institutional actor (institutional/constrained) — maintains system through inertia and risk-aversion; sees own role as degraded (piton perspective)
 *   - Analytical observer: Civilizational view (analytical/analytical) — recognizes genuine coordination entangled with asymmetric extraction (tangled_rope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_federalism_tension, 0.48).
domain_priors:suppression_score(constitutional_federalism_tension, 0.62).
domain_priors:theater_ratio(constitutional_federalism_tension, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_federalism_tension, extractiveness, 0.48).
narrative_ontology:constraint_metric(constitutional_federalism_tension, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_federalism_tension, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_federalism_tension, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(constitutional_federalism_tension, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_federalism_tension, tangled_rope).
narrative_ontology:human_readable(constitutional_federalism_tension, "India's Parallel Personal Law System: Constitutional Federalism Tension").
narrative_ontology:topic_domain(constitutional_federalism_tension, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_federalism_tension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_federalism_tension, '9c13d620-3b71-41a3-a588-2f79ec37b26a').
narrative_ontology:cs_kernel_codification('9c13d620-3b71-41a3-a588-2f79ec37b26a', distributed).
narrative_ontology:cs_authority_grounding('9c13d620-3b71-41a3-a588-2f79ec37b26a', distributed).
narrative_ontology:cs_created_at('9c13d620-3b71-41a3-a588-2f79ec37b26a', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_federalism_tension, religious_institutional_authorities).
narrative_ontology:constraint_beneficiary(constitutional_federalism_tension, male_heads_of_household_traditional_frameworks).
narrative_ontology:constraint_beneficiary(constitutional_federalism_tension, secular_liberal_elites).
narrative_ontology:constraint_victim(constitutional_federalism_tension, women_in_traditional_communities).
narrative_ontology:constraint_victim(constitutional_federalism_tension, interfaith_couples).
narrative_ontology:constraint_victim(constitutional_federalism_tension, religious_minorities_within_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_federalism_tension, religious_minorities_within_communities).
narrative_ontology:constraint_beneficiary(constitutional_federalism_tension, male_heads_household_traditional).
narrative_ontology:constraint_victim(constitutional_federalism_tension, women_traditional_communities).
narrative_ontology:constraint_vindicates(constitutional_federalism_tension, religious_autonomy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_federalism_tension, cultural_pluralism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women within Hindu, Muslim, Christian, or Parsi traditional communities governed by religious personal law. Bear unequal inheritance rights, polygamy permissions in some frameworks, divorce asymmetries, and custody disadvantages relative to secular civil code. Identity-fused with religious community — exit to civil marriage framework requires abandoning family network, social standing, and self-concept as member of tradition. Structurally mobile in principle but functionally locked by internalized identity frame.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, women_traditional_communities, payer,
    powerless, biographical, identity_locked, national).

% Couples from different religious backgrounds seeking legal marriage. Must choose one partner's religious framework or opt into Special Marriage Act (secular civil code). The choice extracts: adopting one religious framework means the other partner formally converts or accepts that tradition's jurisdiction; opting for civil code carries social stigma and family rejection costs in traditional communities. High exit cost but not identity-locked — can access pathways at the price of family conflict and community disapproval.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, interfaith_couples, payer,
    moderate, biographical, constrained, national).

% Hindu religious bodies, temple authorities, and orthodox advocacy organizations. Set and interpret Hindu personal law provisions within the Hindu Marriage Act framework. Preserve jurisdictional authority over family matters for Hindu citizens. Can arbitrage: advocate for secular law when favorable to institutional interests, defer to religious autonomy when challenged. Primary beneficiaries of the parallel system's preservation of religious authority against state secular encroachment.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, religious_authorities_hindu, agenda_setter,
    institutional, generational, arbitrage, national).

% Islamic scholars, All India Muslim Personal Law Board, mosque leadership. Set and interpret Muslim personal law (Sharia application in family matters). The Shah Bano case (1985) exemplifies their structural position: when Supreme Court applied secular maintenance law to Muslim divorce, they successfully lobbied Parliament to reverse the ruling and preserve Sharia jurisdiction. Arbitrage capacity: engage secular legal system to defend religious autonomy. Primary beneficiaries.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, religious_authorities_muslim, agenda_setter,
    institutional, generational, arbitrage, national).

% Christian denominations (Catholic, Protestant, Orthodox) and Parsi religious authorities. Smaller populations than Hindu/Muslim but same jurisdictional structure — administer personal law for their communities under denominational or Parsi Marriage and Divorce Act frameworks. Regional scope because concentrated in specific states (Christians in Kerala, Goa, Northeast; Parsis in Mumbai). Same beneficiary position as larger religious authorities but less political leverage.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, religious_authorities_christian_parsi, agenda_setter,
    institutional, generational, arbitrage, regional).

% Urban educated professional class, secular activists, cultural commentators. Access Special Marriage Act (civil code) without social penalty — their milieu rewards secular choice as modern and progressive. Use the parallel system's existence as evidence that India is pluralist rather than majoritarian. Collect symbolic benefits (cultural sophistication, pluralist credential) without bearing the system's costs (can avoid religious frameworks entirely). Not agenda-setters but beneficiaries of the arrangement's persistence as pluralist theater.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, secular_liberal_elites, beneficiary,
    institutional, immediate, arbitrage, national).

% Shi'a Muslims in Sunni-majority regions, Dalit Christians facing caste discrimination within Christian communities, Parsi reformers challenging orthodoxy. Dual position: benefit from the parallel system's preservation of minority religious autonomy against Hindu majoritarianism (if the state imposed a Hindu-derived civil code, minority traditions would lose jurisdictional protection), but bear extraction when their tradition's establishment interprets doctrine conservatively and they have no secular escape without losing minority-community protection. Constrained exit — leaving means losing the very autonomy the system was meant to preserve.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, religious_minorities_within_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_federalism_tension, religious_minorities_within_communities, beneficiary).

% Coalition of women's rights organizations, secular activists, and Hindu nationalist groups (with different motivations) advocating for unified civil code to replace religious personal laws. Not payers or beneficiaries of the current system — they are building an alternative. See the parallel system as temporary, with sunset logic: as education, urbanization, and constitutional equality norms mature, religious personal law will become obsolete. Whether their alternative represents gender-egalitarian reform or majoritarian cultural assimilation is contested within and outside the movement.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, uniform_civil_code_movement, observer,
    organized, generational, mobile, national).

% Supreme Court and High Courts adjudicating disputes between religious personal law and constitutional secular principles. Maintain the parallel system through precedent and deference to religious autonomy doctrine. Constrained exit — cannot unilaterally dismantle the system without triggering communal conflict and political backlash (Shah Bano reversal demonstrated this limit). See their own role as degraded: the original coordination function (preserving communal peace post-Partition) has atrophied into political risk-aversion. Sustain the system not because it works, but because dismantling it is too destabilizing.
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, constitutional_courts, agenda_setter,
    institutional, civilizational, constrained, national).

% Male heads of household in Hindu, Muslim, Christian, or Parsi communities governed by personal law. Benefit from gender-asymmetric provisions: inheritance priority, unilateral divorce rights in some frameworks, custody presumptions, property control. Not institutional authorities (do not set the rules) but beneficiaries of the rules' operation. Mobile exit — can opt into civil code if advantageous, but rarely do because personal law frameworks favor their position. Secondary beneficiaries (primary benefit flows to religious authorities who preserve the system; male householders benefit derivatively).
narrative_ontology:constraint_stakeholder(constitutional_federalism_tension, male_heads_household_traditional, beneficiary,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The parallel personal law system solves the problem of how to govern family matters in a religiously plural post-colonial state without either (a) imposing one religious tradition's norms on all citizens, or (b) imposing a fully secular framework that religious communities would perceive as cultural erasure. Multiple pathways exist: Hindu, Muslim, Christian, Parsi, and civil frameworks provide legal recognition of marriage and family relationships for populations that could not coordinate through a single unified code.
% TRANSFER_FUNCTION: The system transfers jurisdictional authority over family law from the state to religious institutional authorities. It transfers legal rights asymmetrically: women under personal law frameworks receive fewer inheritance, divorce, custody, and property rights than women under the secular civil code. It transfers symbolic legitimacy: religious authorities collect recognition as autonomous lawmakers; secular elites collect pluralist credentials; the state collects communal peace (or avoids communal conflict). Money, status, and legal power flow from identity-locked populations (women in traditional communities) toward religious establishments and male heads of household.
% ABSENT_VOICES: Women in traditional communities who would prefer secular civil code protections but cannot access them without community expulsion. Religious reformers within traditions (feminist Muslim scholars, Dalit Christian activists, Parsi modernizers) who see personal law as ossified but cannot change it because orthodoxy controls the institutional apparatus. These voices are not in the room because personal law boards and religious authorities are male-dominated and self-perpetuating. Their absence means the coordination consensus (religious autonomy must be preserved) is produced by those who benefit from preservation, not by those who bear its costs. This is commentary-grade provenance (R3): the consensus arose partly because dissenting seats were structurally excluded, not because the reading is universally accepted.
% DISAPPEARANCE_RATIONALE: If the parallel personal law system disappeared overnight and all citizens defaulted to a unified civil code, massive rearrangement would follow: religious authorities would lose jurisdictional power; women in traditional communities would gain constitutional equality protections but face family/community rupture; interfaith couples would no longer face forced choice; male heads of household would lose gender-asymmetric advantages; courts would face jurisdictional simplification but political backlash. The rearrangement would be immediate and structural — legal rights, institutional authority, and family arrangements depend on the system's persistence.
% FOUNDING_PROBLEM: The parallel personal law system was built to solve the communal violence and religious-nationalist conflict of the 1947 Partition. Hindu-Muslim riots had killed hundreds of thousands; Pakistan had been created as a Muslim state; India's founders feared that imposing a Hindu-derived or fully secular civil code would alienate Muslim and Christian minorities and trigger further communal conflict. Preserving religious autonomy over family law was a compromise to prevent religious minorities from perceiving the new state as majoritarian. The founding problem was: how to consolidate a multi-religious nation-state without either civil war or cultural assimilation.
% FOUNDING_PROBLEM_CORROBORATION: The status is contested because different seats evaluate it differently. Secular reformers and women's rights groups argue the founding problem is dead — India has survived 75 years without Partition-scale communal violence, and constitutional norms have matured to the point where unified civil code would not trigger disintegration. Religious authorities and minority-rights advocates argue the founding problem remains live — communal tensions persist (2002 Gujarat riots, ongoing Hindu-Muslim tensions), and dismantling religious autonomy would re-trigger the majoritarianism fear that Partition was meant to resolve. Corroboration: historians and political scientists (Granville Austin, Rochana Bajpai, Pratap Bhanu Mehta) document that the Partition compromise was real and that communal peace was the genuine founding concern. But whether that concern remains structurally valid today is contested. Women within traditional communities (the primary payers) largely corroborate that the problem is dead — they see personal law as patriarchal relic, not communal protection. Religious authorities (primary beneficiaries) insist the problem is live — they cite any instance of communal tension as evidence that autonomy must be preserved.
narrative_ontology:disappearance_verdict(constitutional_federalism_tension, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_federalism_tension, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN IN TRADITIONAL COMMUNITIES (SNARE) — Identity-locked within religious framework that constitutes their social identity and family bonds. Cannot exit without abandoning community, kinship network, and self-concept. Bear maximum extraction: unequal inheritance, polygamy permissions, divorce asymmetries, custody disadvantages. The parallel system prevents them from accessing secular protections available to women in civil marriage frameworks. Structurally mobile (could convert to civil marriage in principle) but identity-fused — exit would require becoming a different person in a different community.
constraint_indexing:constraint_classification(constitutional_federalism_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERFAITH COUPLES (TANGLED ROPE) — Constrained by the requirement to choose one religious framework or opt into the Special Marriage Act (secular civil code), which carries social stigma and family rejection costs. Benefit from the coordination function (multiple pathways exist) but bear extraction through forced choice, documentation barriers, and social penalty for 'betraying' religious identity. Can exit at high cost — the constraint coordinates access to legal marriage but extracts through the necessity of rejecting one partner's tradition.
constraint_indexing:constraint_classification(constitutional_federalism_tension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTIONAL AUTHORITIES (ROPE) — Primary beneficiaries. The parallel system preserves their jurisdictional authority over family law, preventing state encroachment into doctrinal interpretation. Experience the constraint as pure coordination: it solves the genuine problem of how to govern a religiously plural society without imposing one tradition's norms on others. Can arbitrage between religious and secular frameworks when advantageous (clergy can advocate for secular law in cases favorable to institutional interests). Net beneficiary — the system preserves their structural power.
constraint_indexing:constraint_classification(constitutional_federalism_tension, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR LIBERAL ELITES (ROPE) — Benefit from the system's existence as a demonstration of pluralist commitment and cultural sophistication. Can access civil marriage framework without penalty (their social milieu rewards secular choice). Use the parallel system's persistence as evidence that India is not a majoritarian Hindu state. Experience minimal extraction — they are structurally positioned to avoid the system's costs while claiming its symbolic benefits.
constraint_indexing:constraint_classification(constitutional_federalism_tension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS MINORITIES WITHIN COMMUNITIES (TANGLED ROPE) — Shi'a Muslims in Sunni-majority regions, Dalit Christians, Parsi reformers. Benefit from the system insofar as it preserves their tradition's autonomy against majoritarian Hinduism, but bear extraction when their tradition's establishment interprets doctrine conservatively. Constrained exit: leaving the religious framework means losing minority-community protections. Mixed coordination (preserves minority tradition) and extraction (internal hierarchy within the protected space).
constraint_indexing:constraint_classification(constitutional_federalism_tension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: UNIFORM CIVIL CODE MOVEMENT (SCAFFOLD) — Organized coalition (women's rights groups, secular activists, Hindu nationalist factions with different motivations) sees the parallel system as a temporary arrangement until a unified civil code replaces religious personal laws. The sunset logic: as education increases, urbanization proceeds, and constitutional equality norms mature, religious personal law will become vestigial. Whether this coalition genuinely seeks gender-egalitarian reform or majoritarian assimilation is contested — but both factions see the current system as transitional.
constraint_indexing:constraint_classification(constitutional_federalism_tension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL COURTS (PITON) — The judiciary maintains the parallel system through precedent and deference to religious autonomy doctrine, but the original coordination function (preserving communal peace post-Partition) has atrophied. What remains is institutional inertia and political risk-aversion: dismantling the system would trigger communal conflict, so courts preserve it theatrically through narrow rulings and deferral. The courts see their own maintenance of the system as degraded — sustained not because it works, but because alternatives are too destabilizing to pursue.
constraint_indexing:constraint_classification(constitutional_federalism_tension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the system exhibits both genuine coordination (solving the problem of how to govern religious pluralism without coercion) and substantial extraction (gender inequality, exit barriers, jurisdictional complexity). The analytical observer recognizes that the system is not a natural law (it is a contingent institutional compromise) but also not pure extraction (it does solve a real coordination problem that post-colonial states face). The classification as tangled_rope reflects structural ambiguity: the coordination function is real but entangled with asymmetric power distribution.
constraint_indexing:constraint_classification(constitutional_federalism_tension, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_federalism_tension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_federalism_tension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_federalism_tension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_federalism_tension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_federalism_tension, TR),
    TR >= 0.70.

:- end_tests(constitutional_federalism_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The system extracts substantially from women in traditional communities (unequal inheritance, polygamy permissions, divorce asymmetries) and from interfaith couples (forced choice, documentation barriers). But the extraction is not maximal — some coordination function genuinely exists (the system does solve the problem of governing religious pluralism), and some agents (religious authorities, secular elites) experience net benefits. The Shah Bano reversal (1987) marks the steepest extraction increase — women lost constitutional protections to preserve religious autonomy. Suppression (0.62): Moderate-high. Identity-locked women face internalized suppression (cannot imagine exit without losing self-concept); interfaith couples face social ostracism and family rejection costs; minorities within communities face loss of community protection if they exit. But suppression is not total — civil marriage pathways exist, and urban educated populations increasingly access them. The suppression requirement rose sharply after Shah Bano (enforcement hardened to prevent similar challenges) and has remained elevated. Theater ratio (0.38): Moderate. The system is not primarily performative — courts do adjudicate real disputes, and religious authorities do apply doctrinal frameworks to family matters. But theater has increased since Partition: initial functionality (preventing communal violence during religious-nationalist conflict) has partly degraded into political risk-aversion, and courts increasingly defer rather than adjudicate. The Shah Bano reversal was partly theatrical (legislative override to demonstrate religious sensitivity). Theater remains moderate because the system still performs real coordination work, unlike the piton endpoint where performance dominates function.
 *
 * PERSPECTIVAL GAP:
 *   The parallel personal law system produces extreme perspectival divergence because observers occupy radically different structural positions. Women in traditional communities see pure extraction (snare) — they are identity-locked, bear maximum gender inequality, and cannot access secular protections. Religious authorities see pure coordination (rope) — the system preserves their jurisdictional autonomy and prevents state overreach into doctrine. Secular elites also see coordination (rope) — they access civil frameworks easily and claim the system's existence as pluralist virtue. Interfaith couples see mixed coordination and extraction (tangled_rope) — pathways exist but force painful rejection of one partner's tradition. The Uniform Civil Code movement sees a temporary scaffold with sunset logic, though whether that sunset represents reform or assimilation is contested. Constitutional courts see their own degraded maintenance (piton) — the system persists through inertia, not function. The analytical observer sees tangled_rope — genuine coordination solving a real pluralism problem, but entangled with asymmetric power distribution that concentrates extraction on identity-locked populations. No single type is 'correct' — the presheaf over these observation sites IS the structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural relationship to the constraint. Women in traditional communities are victims with identity_locked exit — high d, maximum experienced extraction. Interfaith couples are partly victims (forced choice extracts) and partly coordinated (pathways exist) with constrained exit — moderate-high d. Religious authorities are primary beneficiaries with arbitrage exit — low d, negative or minimal chi (they experience the system as net benefit). Secular elites are secondary beneficiaries with arbitrage exit — low d. Minorities within communities are mixed (benefit from autonomy, bear internal hierarchy) with constrained exit — moderate d. The UCC movement is organized with mobile exit (they can build alternative systems) — moderate-low d. Courts are institutional with constrained exit (cannot easily dismantle the system) but not direct victims — moderate d. The analytical observer's d is context-neutral. The perspectival gap emerges from these divergent directionalities: beneficiaries with arbitrage options see coordination; victims with identity_locked or constrained exit see extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The parallel personal law system resolves the mandatrophy by demonstrating that tangled_rope is the structurally accurate classification at the analytical level — the system genuinely coordinates (solves the pluralism problem) AND genuinely extracts (concentrates gender inequality on identity-locked populations). The mountain classification (inherent to religious pluralism) is a false summit — the system is a contingent institutional compromise, not a natural law. The rope classification from beneficiaries is their legitimate experience but incomplete. The snare classification from victims is their legitimate experience but also incomplete. The scaffold classification from the UCC movement is aspirational — the sunset may or may not arrive, and its nature (reform vs assimilation) is contested. The piton classification from courts captures institutional degradation — the coordination function has partly atrophied into risk-averse deferral. The constraint's identity as tangled_rope prevents mislabeling: it is neither pure coordination (which would erase victims' experience) nor pure extraction (which would erase the genuine pluralism problem the system solves). The analytical observer recognizes both structural features simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Does the parallel personal law system instantiate one contested kernel (marriage/family authority) read through multiple religious traditions, or fundamentally incoherent bundles of distinct kernels that happen to coexist without logical relation?',
    'Structural analysis of whether the religious frameworks share a common question (who has authority over family matters?) with different answers, or ask fundamentally different questions. Test: if one framework were removed, would the others'' legitimacy claims change? If yes, they share a kernel. If no, they are independent kernels.',
    'If one kernel: the constraint is a committer-axis case where different readings foreclose or coexist. If multiple kernels: the constraint is a spatial-coexistence case (multiple independent commitment systems occupying the same territory) with different classification logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Whether parallel personal laws instantiate one contested kernel or multiple independent kernels').

omega_variable(
    sunset_direction_ambiguity,
    'If the Uniform Civil Code movement succeeds, does the resulting unified framework represent gender-egalitarian reform or majoritarian cultural assimilation?',
    'Analysis of UCC coalition composition and historical precedent: which provisions are prioritized in legislative proposals, whose norms are preserved, whose are eliminated. Comparative study of post-colonial states that unified family law (Turkey, Tunisia) — did gender equality or cultural homogenization dominate?',
    'If reform: scaffold perspective validated, sunset is real progress. If assimilation: scaffold perspective is aspirational cover for tangled_rope or snare, sunset replaces one extraction mechanism with another.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_direction_ambiguity, empirical, 'Whether UCC sunset represents reform or assimilation').

omega_variable(
    identity_lock_vs_structural_trap,
    'For women in traditional communities, is the binding mechanism primarily identity-fusion (cannot imagine exit without losing self) or structural barriers (economic dependency, social ostracism, legal obstacles)?',
    'Post-exit trajectory analysis: women who do leave religious frameworks — do they report primarily psychological/identity costs or primarily material/social costs? Longitudinal data on women who converted to civil marriage: what proportion return to religious framework, and why?',
    'If identity-locked: the suppression is internalized, and the constraint''s effective suppression is higher than structural measures suggest (the target carries the lock with them). If structurally trapped: removing material barriers (economic support, anti-discrimination law, community acceptance) would change exit capacity immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether women''s binding is identity-based or structurally material').

omega_variable(
    coordination_vs_jurisdictional_capture,
    'Is the parallel system''s preservation of religious autonomy a genuine coordination solution (preventing state overreach into doctrinal matters), or jurisdictional capture (religious establishments extracting rent from the state''s delegation of family-law authority)?',
    'Comparative institutional analysis: do religious authorities use their jurisdictional power primarily to preserve doctrinal integrity (refuse to perform interfaith marriages, maintain theological coherence) or primarily to extract rents (charge fees, control property, maintain patriarchal authority)? Test: if the state offered full doctrinal autonomy without family-law jurisdiction, would religious institutions accept?',
    'If coordination: the rope perspective from religious authorities is structurally accurate — they are solving a real problem. If capture: the rope perspective is beneficiary rationalization, and the system is more extractive than the analytical classification suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_jurisdictional_capture, empirical, 'Whether religious autonomy represents coordination or rent extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_federalism_tension, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1947_partition, constitutional_federalism_tension, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_1962_secular_consolidation, constitutional_federalism_tension, theater_ratio, 15, 0.3).
narrative_ontology:measurement(theater_1977_shah_bano_prelude, constitutional_federalism_tension, theater_ratio, 30, 0.35).
narrative_ontology:measurement(theater_1987_shah_bano_reversal, constitutional_federalism_tension, theater_ratio, 40, 0.45).
narrative_ontology:measurement(theater_2002_communal_violence, constitutional_federalism_tension, theater_ratio, 55, 0.38).
narrative_ontology:measurement(theater_2022_contemporary, constitutional_federalism_tension, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(extract_1947_partition, constitutional_federalism_tension, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_1962_secular_consolidation, constitutional_federalism_tension, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(extract_1977_shah_bano_prelude, constitutional_federalism_tension, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(extract_1987_shah_bano_reversal, constitutional_federalism_tension, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(extract_2002_communal_violence, constitutional_federalism_tension, base_extractiveness, 55, 0.48).
narrative_ontology:measurement(extract_2022_contemporary, constitutional_federalism_tension, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1947_partition, constitutional_federalism_tension, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(suppress_1987_shah_bano_reversal, constitutional_federalism_tension, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(suppress_2022_contemporary, constitutional_federalism_tension, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_federalism_tension, identity_coordination).

% DUAL FORMULATION NOTE:
% The parallel personal law system is a unified constraint story because it describes one institutional arrangement with one extractiveness value reflecting the aggregate asymmetry across all frameworks. Individual religious frameworks (Hindu Marriage Act, Muslim Personal Law, etc.) could be decomposed into separate constraint stories with their own epsilon values if the analysis focused on within-tradition dynamics rather than the cross-tradition jurisdictional lattice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_federalism_tension, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
