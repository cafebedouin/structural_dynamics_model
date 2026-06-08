% ============================================================================
% CONSTRAINT STORY: hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hindu_codified_reading, []).

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
 *   constraint_id: hindu_codified_reading
 *   human_readable: Hindu Marriage Authority via Parliamentary Codification
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The Hindu Marriage Act (1955) and subsequent amendments represent one
 *   reading of how marriage authority should be grounded in post-colonial
 *   India. This reading holds that parliamentary legislation, claiming to
 *   'codify' traditional dharmashastra principles, is the legitimate source
 *   of marriage law for Hindus, enforced by secular courts with
 *   constitutional override capacity. The constraint exhibits tangled rope
 *   structure: genuine coordination (uniform legal framework, gender reforms,
 *   interstate portability) coexists with extraction (displacement of
 *   traditional religious interpretive authority, procedural costs,
 *   preservation of patriarchal elements under 'tradition' framing). The
 *   codification increased extractiveness over the interval (0.35 → 0.48) as
 *   amendments expanded state authority and procedural complexity. Theater
 *   ratio rose modestly (0.25 → 0.38) as the gap between formal legal
 *   equality claims and actual gender outcomes widened. Suppression increased
 *   (0.50 → 0.62) as constitutional override doctrine and judicial precedent
 *   foreclosed religious challenges to parliamentary amendments.
 *
 * KEY AGENTS:
 *   - Parliamentary Legislative Authority: Primary beneficiary (institutional/arbitrage) — codification channels marriage governance through state legislation, expanding parliamentary domain over religious law
 *   - Secular Judiciary: Primary beneficiary (institutional/arbitrage) — all marriage disputes flow through state courts, expanding judicial authority and resources
 *   - Gender Reform Advocates: Mixed beneficiary-victim (organized/constrained) — benefit from codification as reform vehicle; constrained by preserved patriarchal elements and political coalition requirements
 *   - Traditional Dharmic Authorities: Primary victim (moderate/constrained) — interpretive authority displaced by parliamentary codification claiming to represent dharmashastra
 *   - Women Under Uncodified Practices: Primary victim (powerless/trapped) — trapped between traditional norms and formal legal requirements; bear extraction from both systems
 *   - Middle-Class Hindu Families: Mixed beneficiary-victim (moderate/constrained) — benefit from legal clarity and gender reforms; pay through loss of community autonomy and increased state intermediation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hindu_codified_reading, 0.48).
domain_priors:suppression_score(hindu_codified_reading, 0.62).
domain_priors:theater_ratio(hindu_codified_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hindu_codified_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hindu_codified_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hindu_codified_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hindu_codified_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hindu_codified_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(hindu_codified_reading, "Hindu Marriage Authority via Parliamentary Codification").
narrative_ontology:topic_domain(hindu_codified_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hindu_codified_reading, 'ec54e39a-3926-423e-b34c-877239c90f51').
narrative_ontology:cs_kernel_codification('ec54e39a-3926-423e-b34c-877239c90f51', formalized).
narrative_ontology:cs_authority_grounding('ec54e39a-3926-423e-b34c-877239c90f51', lineage).
narrative_ontology:cs_interpretation_layer_present('ec54e39a-3926-423e-b34c-877239c90f51').
narrative_ontology:cs_reading_relation('ec54e39a-3926-423e-b34c-877239c90f51', hindu_codified_reading__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('ec54e39a-3926-423e-b34c-877239c90f51', hindu_codified_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec54e39a-3926-423e-b34c-877239c90f51', hindu_codified_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec54e39a-3926-423e-b34c-877239c90f51', hindu_codified_reading__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('ec54e39a-3926-423e-b34c-877239c90f51', foundational, parliamentary_codification_legitimacy).
narrative_ontology:cs_axiom_status(parliamentary_codification_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ec54e39a-3926-423e-b34c-877239c90f51', parliamentary_codification_legitimacy, conventional).
narrative_ontology:cs_axiom('ec54e39a-3926-423e-b34c-877239c90f51', foundational, dharmic_continuity_through_reform).
narrative_ontology:cs_axiom_status(dharmic_continuity_through_reform, holdable).
narrative_ontology:cs_axiom_grounding('ec54e39a-3926-423e-b34c-877239c90f51', dharmic_continuity_through_reform, conventional).
narrative_ontology:cs_axiom('ec54e39a-3926-423e-b34c-877239c90f51', secondary, constitutional_override_of_religious_law).
narrative_ontology:cs_axiom_status(constitutional_override_of_religious_law, holdable).
narrative_ontology:cs_axiom_grounding('ec54e39a-3926-423e-b34c-877239c90f51', constitutional_override_of_religious_law, conventional).
narrative_ontology:cs_reference_frame('ec54e39a-3926-423e-b34c-877239c90f51', dharmashastra_parliamentary_synthesis).
narrative_ontology:cs_drift_state('ec54e39a-3926-423e-b34c-877239c90f51', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec54e39a-3926-423e-b34c-877239c90f51', '').
narrative_ontology:cs_kernel_id(hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hindu_codified_reading, parliamentary_legislative_authority).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, secular_judiciary).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, gender_reform_advocates).
narrative_ontology:constraint_victim(hindu_codified_reading, traditional_dharmic_authorities).
narrative_ontology:constraint_victim(hindu_codified_reading, women_in_uncodified_practices).
narrative_ontology:constraint_victim(hindu_codified_reading, religious_autonomy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, middle_class_hindu_families).
narrative_ontology:constraint_victim(hindu_codified_reading, gender_reform_advocates).
narrative_ontology:constraint_victim(hindu_codified_reading, middle_class_hindu_families).
narrative_ontology:constraint_vindicates(hindu_codified_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(hindu_codified_reading, constitutional_override_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parliament sets the marriage law framework through codification and amendment. Controls the legislative agenda: can expand or contract the scope of codified marriage law, can override traditional religious interpretations through constitutional amendment. Can arbitrage between strict codification (claiming fidelity to dharmashastra) and progressive reform (invoking constitutional equality) depending on political coalition requirements.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, parliamentary_legislative_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts enforce the codified marriage law and adjudicate disputes. All marriage cases flow through the judicial system, expanding institutional authority and resources. Can arbitrage between strict textual interpretation of the Act and progressive constitutional reading (fundamental rights override) depending on case facts and judicial philosophy.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, secular_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized coalition (women's rights NGOs, progressive legislators, feminist legal scholars) that uses codification as a reform vehicle. Benefits from amendments expanding women's rights (1976 divorce grounds, maintenance provisions, property rights). Pays through the constraint's preservation of patriarchal elements (restitution of conjugal rights, limited divorce grounds) and through political coalition costs required to pass amendments. Can influence through legislative lobbying and public interest litigation but constrained by religious conservative opposition.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, gender_reform_advocates, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hindu_codified_reading, gender_reform_advocates, payer).

% Religious scholars, temple authorities, community elders whose interpretive authority over marriage was displaced by parliamentary codification. The Act claims to 'codify' dharmashastra but actually reforms it, appropriating their traditional governance role. Can exit by refusing to perform marriages under the Act, but this marginalizes them from the majority Hindu community. Constitutional override doctrine forecloses religious challenges to parliamentary amendments.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, traditional_dharmic_authorities, payer,
    moderate, biographical, constrained, regional).

% Women whose marriages operate under traditional dharmic norms that the codification claims to supersede but which persist in practice. Trapped between two systems: traditional authorities extract through social enforcement of uncodified norms; state extracts through procedural complexity, court fees, and legal intermediation requirements. Cannot navigate formal legal system without institutional help. Cannot exit the marriage system entirely without social death.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, women_in_uncodified_practices, payer,
    powerless, biographical, trapped, regional).

% Families that benefit from standardized marriage registration, divorce procedures, inheritance rules, and gender reforms. Pay through legal fees, procedural delays, and loss of traditional community arbitration mechanisms. Can exit to secular marriage under Special Marriage Act but at significant social cost (family opposition, community exclusion). Mixed position: coordination benefits are real, but so are extraction costs.
narrative_ontology:constraint_stakeholder(hindu_codified_reading, middle_class_hindu_families, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hindu_codified_reading, middle_class_hindu_families, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform legal framework for marriage governance across India's Hindu population (80% of 1.4 billion people). Standardizes marriage registration, divorce procedures, maintenance obligations, inheritance rights, and property division. Enables interstate portability of marriage status. Serves as vehicle for gender reform through parliamentary amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority over marriage law from traditional religious institutions (temple authorities, dharmic scholars, community elders) to parliamentary legislation and secular judiciary. Transfers procedural control from community arbitration to state courts. Transfers resources from families to legal system (court fees, lawyer costs). Transfers some rights from husbands to wives through gender reform amendments (divorce grounds, maintenance, property).
% ABSENT_VOICES: Uncodified traditional practitioners (rural communities, lower castes, tribal groups) whose marriage practices fall outside both traditional brahminical dharmashastra and parliamentary codification. Dalit and tribal communities whose customary marriage practices were never represented in classical dharmic texts and are now displaced by codification claiming to represent 'Hindu' tradition. These voices are absent from both the traditional authority structure and the parliamentary reform coalition.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act disappeared overnight, marriage governance would not revert to a stable traditional system — it would fragment. Urban middle-class families would migrate to Special Marriage Act (secular contract). Traditional authorities would compete for interpretive authority with no unified framework. Women's rights advocates would lose the legislative reform vehicle. Courts would lose jurisdiction over 80% of marriage disputes. The institutional arrangements (parliamentary authority over religious law, judicial enforcement, gender reform amendments) depend on the codification's existence.
% FOUNDING_PROBLEM: Post-colonial India faced the problem of governing marriage for a Hindu population (80% of the country) that had no unified traditional marriage law. Classical dharmashastra texts (Manusmriti, Yajnavalkya, Narada) were diverse, contradictory, and brahminical (excluding lower castes and tribal groups). British colonial law had created a patchwork of precedents. The founding problem was: how to create uniform marriage law for Hindus that could be enforced by secular courts, enable gender reform, and claim continuity with tradition.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between two camps. Parliamentary reformers and feminist legal scholars argue the problem is LIVE: India still lacks uniform civil code, gender inequality persists in marriage law, and legal pluralism creates jurisdictional complexity. They cite ongoing legislative amendments (most recently 2024 proposals for gender-neutral marriage laws) as evidence the founding problem remains unresolved. Traditional dharmic authorities and religious autonomy advocates argue the problem is DEAD or was misframed: the 'problem' of diverse traditional practices was not a problem requiring state solution but a feature of Hindu legal pluralism. They cite the persistence of uncodified practices and community arbitration as evidence that codification solved a problem the state created rather than one that existed. Corroboration: Law Commission of India reports (1956, 2018) acknowledge ongoing contestation; Supreme Court judgments (Sarla Mudgal 1995, John Vallamattom 2003) cite the founding problem as justification for continued reform, while dissenting opinions question whether uniformity was ever the appropriate goal.
narrative_ontology:disappearance_verdict(hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(hindu_codified_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER UNCODIFIED PRACTICES (SNARE) — Trapped between traditional dharmic norms (which the codification claims to supersede but which persist in practice) and formal legal requirements they cannot navigate without institutional intermediaries. Bear extraction from both systems: traditional authorities extract through social enforcement; state extracts through procedural complexity and court fees. No genuine exit — leaving the marriage system entirely means social death.
constraint_indexing:constraint_classification(hindu_codified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS HINDU FAMILIES (TANGLED ROPE) — Experience genuine coordination (standardized marriage registration, divorce procedures, inheritance rules) alongside extraction (legal fees, procedural delays, loss of traditional community arbitration). Can exit to secular marriage under Special Marriage Act but at significant social cost. Mixed beneficiary-victim: benefit from gender reforms and legal clarity; pay through loss of community autonomy and increased state intermediation.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SECULAR JUDICIARY (ROPE) — Primary beneficiary. Codification channels all marriage disputes through state courts, expanding judicial authority and institutional resources. Experiences the constraint as coordination: uniform legal framework enables consistent adjudication. Can arbitrage between strict textual interpretation and progressive constitutional reading depending on case facts. Net beneficiary — extraction flows toward judicial institutional power.
constraint_indexing:constraint_classification(hindu_codified_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GENDER REFORM ADVOCATES (TANGLED ROPE) — Organized coalition (women's rights NGOs, progressive legislators, feminist legal scholars) that benefits from codification as a reform vehicle (1976 divorce grounds expansion, maintenance provisions, property rights) but also experiences extraction through the constraint's preservation of patriarchal elements (restitution of conjugal rights, limited grounds for divorce compared to secular marriage). Can influence through legislative amendment but constrained by political coalition requirements and religious conservative opposition.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL DHARMIC AUTHORITIES (SNARE) — Religious scholars, temple authorities, community elders whose interpretive authority was displaced by parliamentary codification. Experience the constraint as extraction: their traditional role in marriage governance was appropriated by state legislation claiming to 'codify' dharmashastra while actually reforming it. Can exit by refusing to perform marriages under the Act, but this marginalizes them from the majority Hindu community. Suppression is high: constitutional override doctrine forecloses religious challenge to parliamentary amendments.
constraint_indexing:constraint_classification(hindu_codified_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a comparative constitutional perspective, this reading instantiates a specific resolution of the religion-state tension: parliamentary supremacy over religious law, with judicial enforcement and constitutional override. Genuine coordination function exists (uniform marriage law for 80% of population, gender reform vehicle, interstate portability) alongside structural extraction (displacement of religious interpretive authority, procedural costs, preservation of patriarchal elements as 'tradition'). The codification is neither pure coordination (rope) nor pure extraction (snare) but a hybrid where the state coordinates marriage governance while extracting authority from traditional religious institutions.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hindu_codified_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hindu_codified_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hindu_codified_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The codification extracts authority from traditional religious institutions while providing genuine coordination benefits (uniform framework, gender reforms). The extraction is substantial but not maximal — the coordination function is real, not pure cover. Initial value (0.35) reflected the Act's early period when it was closer to genuine codification; the increase to 0.48 reflects accumulation of amendments that expanded state authority beyond traditional dharmic principles. Suppression (0.62): Moderate-high. Constitutional override doctrine (Article 13: laws inconsistent with fundamental rights are void) forecloses religious challenges to parliamentary amendments. Traditional authorities cannot exit without marginalizing themselves. Women under uncodified practices face high barriers to accessing formal legal system. The suppression increased over the interval as judicial precedent hardened the override doctrine. Theater ratio (0.38): Moderate. The gap between formal legal equality claims and actual outcomes is significant but not dominant. The Act provides real procedural mechanisms (divorce grounds, maintenance, property rights) that function beyond theater, but the preservation of patriarchal elements (restitution of conjugal rights, limited divorce grounds compared to secular marriage) under 'tradition' framing is partly performative. The theater ratio increased modestly as the gap between constitutional equality rhetoric and gendered legal outcomes widened.
 *
 * PERSPECTIVAL GAP:
 *   The secular judiciary sees rope (coordination via uniform legal framework). Gender reform advocates see tangled rope (genuine reform vehicle with embedded patriarchal preservation). Traditional dharmic authorities see snare (authority displacement under codification cover). Women under uncodified practices see snare (trapped between two extractive systems). Middle-class families see tangled rope (mixed coordination and extraction). The analytical observer sees tangled rope (genuine coordination function coexisting with structural extraction from religious institutions). The gap reveals that the same legal regime appears as coordination to its institutional beneficiaries, as mixed coordination-extraction to those with agency and resources, and as pure extraction to those trapped by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary legislative authority and secular judiciary are primary beneficiaries with arbitrage exit options — they can choose between strict textual interpretation and progressive constitutional reading depending on political context. Their directionality values are low, producing low or negative effective extraction (they collect from the constraint). Traditional dharmic authorities are victims with constrained exit — they can refuse to participate but this marginalizes them. Their directionality is high, producing high effective extraction. Women under uncodified practices are victims with trapped exit — they cannot leave the marriage system without social death and cannot navigate the formal legal system without institutional intermediaries. Their directionality is maximum, producing maximum effective extraction. Middle-class Hindu families are mixed beneficiary-victim with constrained exit — they benefit from legal clarity and gender reforms but pay through loss of community autonomy and increased procedural costs. Their directionality is moderate. Gender reform advocates are organized beneficiaries with constrained exit — they can influence through legislative amendment but are constrained by political coalition requirements. Their directionality is moderate-low.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that codification is neither pure coordination (the traditional authorities' displacement is real extraction) nor pure extraction (the gender reform and legal clarity functions are genuine coordination). The tangled rope classification captures the hybrid structure: parliamentary codification coordinates marriage governance for 80% of India's population while extracting interpretive authority from traditional religious institutions. The mandate (uniform marriage law, gender reform) has not outlived its function — the coordination benefits are real and ongoing. But the extraction is also real and increasing: the state uses gender reform to legitimize authority displacement, and the 'codification' framing naturalizes parliamentary supremacy over religious law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_fidelity_ambiguity,
    'Does the Hindu Marriage Act ''codify'' pre-existing dharmashastra principles or construct a new legal regime under the cover of codification?',
    'Textual comparison of Act provisions with classical dharmashastra texts (Manusmriti, Yajnavalkya, Narada); historical analysis of drafting debates showing reform intent vs. codification intent; tracking of provisions with no dharmic precedent (e.g., statutory divorce grounds).',
    'If genuine codification: lower extractiveness, higher legitimacy for traditional authorities. If constructed regime: higher extractiveness, the ''codification'' framing is a legitimation strategy that naturalizes state authority over religious law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(codification_fidelity_ambiguity, empirical, 'Whether codification is faithful representation or constructed reform').

omega_variable(
    alternative_reading_foreclosure,
    'Does this reading''s parliamentary supremacy premise logically foreclose the Muslim personal law reading, or do they coexist as parallel regimes?',
    'Constitutional analysis of Article 44 (Uniform Civil Code directive) vs. Article 25 (religious freedom); examination of whether parliamentary codification of Hindu law creates precedent that structurally pressures Muslim personal law or whether the two readings occupy separate constitutional spaces.',
    'If forecloses: the Hindu codification reading creates structural momentum toward uniform civil code, making Muslim personal law reading unstable. If coexists: both readings are stable within India''s legal pluralism framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether parliamentary codification of one community''s law forecloses or coexists with another''s uncodified regime').

omega_variable(
    gender_reform_sunset_question,
    'Is the gender reform function of codified Hindu marriage law transitional (scaffold with sunset toward gender-neutral secular marriage) or permanent (tangled rope as steady state)?',
    'Longitudinal analysis of Special Marriage Act adoption rates among Hindus; tracking of legislative amendments to Hindu Marriage Act vs. expansion of secular marriage option; political coalition analysis of whether reform advocates see codification as endpoint or stepping stone.',
    'If transitional: scaffold classification from reform advocate perspective is correct, with sunset as secular marriage becomes normative. If permanent: tangled rope is steady state, and the reform function is embedded extraction (state uses gender reform to legitimize authority displacement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_reform_sunset_question, empirical, 'Whether gender reform via codification is transitional or permanent arrangement').

omega_variable(
    constitutional_override_scope_ambiguity,
    'Does constitutional override doctrine apply symmetrically to all personal law readings, or does it apply asymmetrically (codified Hindu law is amendable by Parliament; uncodified Muslim law is protected by religious freedom)?',
    'Comparative analysis of Shah Bano case (1985) and subsequent Muslim Women Act (1986) vs. Hindu Marriage Act amendments; examination of whether parliamentary override of Muslim personal law faces higher constitutional barriers than override of codified Hindu law.',
    'If symmetric: all readings face equal parliamentary supremacy, and the codification is not extractive relative to alternatives. If asymmetric: codification increases extractive vulnerability — codified communities are more exposed to parliamentary override than uncodified ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_override_scope_ambiguity, empirical, 'Whether constitutional override applies symmetrically across personal law regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcr_theater_1955, hindu_codified_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hcr_theater_1975, hindu_codified_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(hcr_theater_1995, hindu_codified_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hcr_theater_2025, hindu_codified_reading, theater_ratio, 70, 0.38).

% Extraction over time
narrative_ontology:measurement(hcr_extract_1955, hindu_codified_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hcr_extract_1975, hindu_codified_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hcr_extract_1995, hindu_codified_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(hcr_extract_2025, hindu_codified_reading, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hcr_suppress_1955, hindu_codified_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hcr_suppress_1975, hindu_codified_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hcr_suppress_1995, hindu_codified_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(hcr_suppress_2025, hindu_codified_reading, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The hindu_codified_reading is one of five readings of the marriage_authority_kernel. Each reading has its own extractiveness value reflecting its specific institutional structure. The Hindu codification's extractiveness (0.48) reflects the authority displacement from traditional religious institutions; the Muslim personal law reading's extractiveness reflects different structural dynamics (uncodified regime with community interpretation). The readings are linked because parliamentary codification of Hindu law creates structural precedent that pressures uncodified Muslim personal law (omega variable: does codification foreclose or coexist with uncodified regimes?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
