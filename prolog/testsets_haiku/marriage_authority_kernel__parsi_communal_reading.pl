% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Community Marriage Authority (Communal Reading)
 *   domain: legal/constitutional/religious
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies Zoroastrian communal
 *   custom as the authoritative source of marriage legitimacy, property
 *   transmission, and family law for Parsis within India's pluralist
 *   constitutional system. The constraint operates through Parsi matrimonial
 *   courts and fire temples, which administer the statute and enforce
 *   endogamy (marriage within the Parsi faith). The Parsi communal reading
 *   holds that this arrangement is legitimate because it preserves minority
 *   religious identity and self-determination within a secular state, enables
 *   community cohesion through shared legal institutions, and achieves
 *   historically high gender equity in marriage within the Parsi community.
 *   The competing secular civil reading holds that the same constraint is
 *   extractive: endogamy enforcement bars interfaith marriage, compels
 *   individuals to forfeit property and inheritance rights if they marry
 *   outside the community, excludes LGBTQ Parsis from marriage legitimacy
 *   entirely, and contributes to demographic decline by blocking the most
 *   natural demographic strategy (interfaith reproduction) that could sustain
 *   the community. The claim/metric divergence is intentional: the constraint
 *   is CLAIMED as Tangled Rope (genuine coordination of communal marriage
 *   rites + marriage legitimacy + property transmission, combined with
 *   asymmetric extraction of endogamy compliance), and the authored metrics
 *   describe moderate extraction (0.31) with suppression (0.42) rising over
 *   the interval — the engine will compute per-seat divergence; the Parsi
 *   community's seat may compute toward rope (genuine coordination benefit
 *   without substantial personal extraction), while interfaith-couple and
 *   LGBTQ-individual seats compute toward snare (extraction with minimal real
 *   coordination benefit for them).
 *
 * KEY AGENTS:
 *   - parsi_community_collective: administers the constraint, sets endogamy rules, collects social authority and demographic cohesion
 *   - parsi_fire_temples: provide religious legitimacy and ritual witnessing for marriages; maintain Zoroastrian religious basis
 *   - parsi_matrimonial_courts: adjudicate marriage dissolution and property disputes; exercise interpretive authority over the statute
 *   - parsi_women_in_patrilineal_property_disputes: bear asymmetric property transmission costs; constrained exit
 *   - parsi_interfaith_couples: excluded from communal marriage authority; identity-locked to community identity but barred from marriage within it
 *   - parsi_lgbtq_individuals: excluded from marriage legitimacy under the heterosexual-marriage framing of the Act; identity-locked
 *   - secular_civil_authority: represents the competing secular civil reading grounded in constitutional individual rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.31).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Community Marriage Authority (Communal Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "legal/constitutional/religious").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '4973a065-7a38-4e49-a57d-bad824e566b6').
narrative_ontology:cs_kernel_codification('4973a065-7a38-4e49-a57d-bad824e566b6', fixed_text).
narrative_ontology:cs_authority_grounding('4973a065-7a38-4e49-a57d-bad824e566b6', lineage).
narrative_ontology:cs_interpretation_layer_present('4973a065-7a38-4e49-a57d-bad824e566b6').
narrative_ontology:cs_reading_relation('4973a065-7a38-4e49-a57d-bad824e566b6', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('4973a065-7a38-4e49-a57d-bad824e566b6', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4973a065-7a38-4e49-a57d-bad824e566b6', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4973a065-7a38-4e49-a57d-bad824e566b6', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('4973a065-7a38-4e49-a57d-bad824e566b6', foundational, communal_self_determination_grounds_marriage_authority).
narrative_ontology:cs_axiom_status(communal_self_determination_grounds_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('4973a065-7a38-4e49-a57d-bad824e566b6', communal_self_determination_grounds_marriage_authority, deontological).
narrative_ontology:cs_axiom('4973a065-7a38-4e49-a57d-bad824e566b6', foundational, endogamy_preservation_necessary_to_cultural_continuity).
narrative_ontology:cs_axiom_status(endogamy_preservation_necessary_to_cultural_continuity, overridden).
narrative_ontology:cs_axiom_grounding('4973a065-7a38-4e49-a57d-bad824e566b6', endogamy_preservation_necessary_to_cultural_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('4973a065-7a38-4e49-a57d-bad824e566b6', ancestral_zoroastrian_custom_codified_1936).
narrative_ontology:cs_drift_state('4973a065-7a38-4e49-a57d-bad824e566b6', contemporary_constitutional_pluralism_moment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4973a065-7a38-4e49-a57d-bad824e566b6', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_collective).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_fire_temples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_in_patrilineal_property_disputes).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_lgbtq_individuals).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_self_determination).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, religious_law_pluralism).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, minority_cultural_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parsi community through its representative institutions (community councils, fire temples, matrimonial courts) administers the Parsi Marriage and Divorce Act 1936 as the authoritative framework for marriage legitimacy within the community. Sets the rules for who may marry (endogamy requirements), how marriage is dissolved, property division upon death, and custody of children. Claims authority derives from ancestral custom transmitted through religious institutions and 1936 statutory codification. Collects social authority and maintains demographic cohesion through enforcing endogamy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_collective, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_community_collective, beneficiary).

% Bear the asymmetric property consequences of the communal reading: the constraint privileges patrilineal property transmission and male inheritance under Parsi law, limiting women's testamentary and succession rights relative to men, even within the community's own high-equity partnership norms. Exit means leaving the Parsi community entirely (leaving the jurisdiction where Parsi law applies), forfeiting property claims under Parsi succession law and losing community standing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_in_patrilineal_property_disputes, payer,
    powerless, biographical, constrained, national).

% Administer religious rites, witness marriages, and legitimize marriages under Parsi custom. Maintain custodianship of the marriage authority's religious basis. Benefit from demographic cohesion enforced through endogamy (maintained ritual importance, community participation). Oversee the transmission of Zoroastrian identity and religious law through marriage ceremonies and family formation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_fire_temples, agenda_setter,
    powerful, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_fire_temples, beneficiary).

% Adjudicate marriage dissolution, property division, and custody disputes under the Parsi Marriage and Divorce Act. Exercise interpretive authority over the statute and communal custom. Administer enforcement of the constraint through dispute resolution. Maintain the boundary between Parsi personal law and civil law for their constituencies.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_courts, agenda_setter,
    organized, generational, constrained, national).

% Parsi individuals in relationships with non-Parsis face exclusion from communal marriage authority: the Parsi Marriage and Divorce Act 1936 does not recognize interfaith marriages, forcing couples to seek legitimacy under civil law (Special Marriage Act). This creates dual-status: not fully recognized within the community, not fully protected under Parsi succession and property law. Exit from the community identity is identity-locked because Parsi identity is inherited matrilineally; a Parsi marrying out risks children being declassified as non-Parsi.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_interfaith_couples, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_interfaith_couples, excluded).

% Cannot marry under the Parsi Marriage and Divorce Act 1936, which assumes heterosexual marriage and codifies gender roles in property and custody. LGBTQ Parsis must either forgo marriage within the community or seek recognition under civil law (Special Marriage Act or, after 2023, civil partnership frameworks), forfeiting inheritance protections, succession rights, and community standing. Identity-locked: leaving Parsi identity to access marriage rights means losing Zoroastrian religious rites and community participation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_lgbtq_individuals, payer,
    powerless, biographical, identity_locked, national).

% The Parsi population in India has declined from 114,890 in 1941 to approximately 57,000 in 2021. Endogamy enforcement through marriage law contributes to this decline by barring interfaith marriage and reducing reproductive participation. The constraint's enforcement mechanism (exclusive legitimacy for endogamous marriages) structurally contradicts the demographic sustainability it claims to serve.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_demographic_decline, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority_kernel__parsi_communal_reading, parsi_demographic_decline).

% Represents the competing Hindu Codified Reading: the Hindu Marriage Act 1955 and civil courts' interpretive authority over Hindu personal law. This reading subordinates communal custom to statutory codification and civil-court jurisdiction, contrasting sharply with the Parsi communal reading's reliance on community tribunals and religious authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, hindu_statutory_authority, observer,
    institutional, generational, analytical, national).

% Represents the Secular Civil Reading: the Special Marriage Act 1954 and the constitutional framework of individual rights and non-discrimination. This reading grounds marriage authority in constitutional provisions (Articles 14, 15, 21) rather than communal or religious law, and permits interfaith and same-sex marriage. It stands in structural tension with the Parsi communal reading's endogamy enforcement.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_civil_authority, observer,
    institutional, generational, analytical, national).

% An abstract commitment to recognize multiple sources of legal authority (communal, religious, civil) within a single constitutional state. The Parsi communal reading vindicates this framework by instantiating minority religious law pluralism, though its endogamy enforcement creates friction with constitutional individual-rights guarantees.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, constitutional_pluralism_framework, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority_kernel__parsi_communal_reading, constitutional_pluralism_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_community_collective).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage legitimacy, property transmission, and family formation within the Parsi community by vesting authority in communal institutions (fire temples, matrimonial courts) rather than requiring all Parsis to navigate civil law. Solves the problem of maintaining communal religious rites, inheritance custom, and cultural identity within a pluralist constitutional state.
% TRANSFER_FUNCTION: Transfers authority to define marriage legitimacy from the individual and the state to the Parsi community collective. Moves property transmission rights and succession authority through patrilineal lines under Parsi law rather than through secular testamentary freedom. Extracts social conformity (endogamy compliance) from individuals in exchange for community recognition and succession protections.
% ABSENT_VOICES: Parsi interfaith couples and LGBTQ individuals are structurally excluded from the Parsi marriage authority framework. They would argue for individual choice, equal recognition, and removal of endogamy and gender-identity barriers to marriage legitimacy. Their exclusion is not accidental — it is the mechanism by which endogamy is enforced and communal identity is preserved. Secular constitutional authorities and human-rights advocates also remain outside the deliberation, challenging the framework's compatibility with constitutional non-discrimination.
% DISAPPEARANCE_RATIONALE: If the Parsi Marriage and Divorce Act 1936 and communal marriage authority vanished overnight, Parsi marriage legitimacy would collapse into the secular civil framework (Special Marriage Act), Parsi inheritance law would be displaced by intestacy under the Indian Succession Act, fire-temple marriages would lose statutory authority, and matrimonial courts would lose jurisdiction. The Parsi community would lose its institutional basis for administering marriage and property law; interfaith Parsi couples would gain equal recognition; the demographic-decline mechanism (endogamy enforcement) would release. Community cohesion maintained through marriage law would deteriorate unless community institutions reorganized around non-legal mechanisms (social pressure, religious participation).
% FOUNDING_PROBLEM: Early 20th-century Parsi community faced the problem of maintaining religious identity, marriage legitimacy, and property transmission within the Indian colonial and post-colonial legal system, which did not recognize Zoroastrian personal law. The 1936 Act codified communal custom to secure recognition for Parsi marriages and property law within the constitutional framework, enabling the community to self-govern family formation without full assimilation into Hindu or secular civil law.
% FOUNDING_PROBLEM_CORROBORATION: The Parsi community and its religious institutions attest the founding problem remains live: without communal marriage authority, Parsi religious identity and property transmission would be lost. Constitutional-pluralism scholars and minority-rights advocates attest that the founding problem is substantially solved by the 1955 Hindu Marriage Act (which extended codified protections to all communities) and the 1954 Special Marriage Act (which enables interfaith marriage). Human-rights organizations argue the founding problem was never about legal recognition per se, but about demographic and cultural survival — and the Act's endogamy enforcement now CREATES the demographic problem it claims to solve. No outside corroboration supports the endogamy-enforcement framing as necessary to the founding problem's solution.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31) because the constraint delivers genuine coordination benefits (centralized marriage legitimacy, property transmission security) alongside asymmetric costs (endogamy compliance, property asymmetry for women, exclusion for interfaith/LGBTQ individuals). Suppression is measured at 0.42, rising from 0.28 in 1936, because the constraint's persistence depends on active enforcement of endogamy rules and exclusion of interfaith marriages — enforcement capacity has hardened over the interval (matrimonial courts have developed case law restricting interfaith recognition, community councils have formalized endogamy conditions). Theater ratio is low (0.18) because the arrangement's functions (marriage legitimacy, property transmission) remain functionally real; the rising trajectory (0.08 in 1936 → 0.18 in 2026) reflects increasing performative emphasis on cultural preservation as the demographic rationale for endogamy enforcement becomes less convincing. Accessibility collapse is 0.67 because alternatives (civil law marriage under the Special Marriage Act) exist and are legally accessible to Parsis, but Parsis choosing them forfeit community recognition and succession protections — the alternative is cognitively available but structurally costly. Resistance is 0.38 (moderate) because some Parsi individuals and reform advocates challenge the Act's endogamy and gender-equity provisions, but they remain a minority within the community; the majority continues to recognize the constraint's legitimacy as a mechanism for cultural preservation.
 *
 * PERSPECTIVAL GAP:
 *   The parsi_community_collective and parsi_fire_temples should compute toward rope or moderate tangled-rope from their seats: they exercise authority, collect social cohesion benefits, and view the constraint as serving genuine coordination. The parsi_interfaith_couples and parsi_lgbtq_individuals should compute toward snare from their seats: the constraint extracts endogamy compliance and marriage-denial with minimal coordination benefit for them, and the suppression is internalized (they have internalized the belief that being Parsi means accepting these restrictions, or they have accepted identity-based exclusion as inevitable). The parsi_women_in_patrilineal_property_disputes compute toward moderate tangled-rope: they receive marriage legitimacy and some property protections within the Parsi framework, but bear asymmetric patrilineal transmission costs. The secular_civil_authority seat sees the constraint as extractive snare riding a cover story of cultural preservation. These divergences follow from the structural data: different stakeholders benefit from and bear costs of the same constraint differently; the engine's per-seat classification surfaces this automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   The parsi_community_collective has low d (near beneficiary, ~0.15) because it sets the rules, collects social authority and demographic cohesion, and has mobile exit (community institutions could exist under secular law, though weakened). The parsi_fire_temples have similarly low d (~0.20) because they maintain ritual authority and community participation, with somewhat constrained exit (could continue under civil law but with reduced centrality). The parsi_women_in_patrilineal_property_disputes have moderate-to-high d (~0.55) because they bear the asymmetric property costs and have constrained exit (leaving the community forfeits succession rights). The parsi_interfaith_couples have high d (~0.85) because the constraint directly excludes them from marriage legitimacy and forces them to choose between community identity and marriage rights — identity-locked exit means the constraint extracts maximum compliance from them. The parsi_lgbtq_individuals have similarly high d (~0.85) because they face the same binary: accept celibacy within the community or forfeit community status. No directionality overrides are needed; the structural derivation from beneficiary/victim + exit produces the correct directional placement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint manifests classic mandatrophy: the founding problem (securing Parsi legal recognition and marriage legitimacy within a pluralist constitutional state) is substantially solved — the 1955 Hindu Marriage Act extended codified protections to all communities, the 1954 Special Marriage Act enabled interfaith marriage, and the constitutional framework recognizes minority personal law pluralism. Yet the constraint persists with rising suppression (endogamy enforcement intensifying, not relaxing). The rationalization has shifted from 'securing legal recognition for Parsi marriage' (solved in 1954) to 'preserving Parsi cultural and demographic identity' — but the demographic strategy the constraint employs (endogamy enforcement) contradicts the demographic goal (population sustainability). The theater ratio rising from 0.08 to 0.18 reflects this: increasing activity is devoted to performing cultural preservation (discourse about Parsi identity, framing endogamy as cultural rather than legal necessity) while the functional mechanism (actual demographic reproduction) atrophies. The constraint is neither abandoned (community institutions remain invested) nor fixed (endogamy enforcement persists despite demographic evidence of failure); it persists through institutional inertia and the sunk-cost nature of religious identity investment. The remedy would be either: (1) decouple the constraint from demographic claims and accept demographic decline as the cost of endogamy enforcement, or (2) relax endogamy enforcement and accept demographic gains as the consequence. The fact that neither remedy is pursued, despite both the founding problem being solved and the demographic strategy failing, is the classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_preservation_vs_demographic_strategy,
    'Is endogamy enforcement a necessary mechanism for preserving Parsi cultural identity, or a contingent strategy that contradicts the demographic goal it claims to serve?',
    'Comparative analysis of minority communities that achieved cultural preservation without endogamy enforcement (e.g., Jewish diaspora communities with high interfaith marriage rates); survey data on Parsi attitudes toward marriage, identity, and community participation; modeling of alternative demographic strategies (interfaith marriage + religious participation).',
    'If endogamy is NOT necessary to cultural preservation, the constraint''s mandate has expired and the mechanism (exclusion, suppression) has become pure extraction riding a false rationale. The classification would shift from tangled-rope to snare. If endogamy IS necessary (empirically or normatively), the extraction is justified as cultural preservation cost, and the tangled-rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_preservation_vs_demographic_strategy, empirical, 'Whether endogamy enforcement is causally necessary to Parsi cultural preservation.').

omega_variable(
    communal_vs_individual_authority_source,
    'Does the legitimacy of Parsi marriage authority derive from Parsi communal custom (communal reading), or from constitutional recognition of minority self-determination (secular reading)? Does the community''s authority depend on the custom being ancestral and pre-constitutional, or on the modern constitutional pluralism framework?',
    'Historical analysis of whether Parsi community institutions administered marriage law before 1936, or whether the authority was constructed by the codification itself; examination of how Parsi institutions justify authority to younger generations (ancestral transmission vs. constitutional recognition); analysis of whether the 1936 Act''s authority persists because of communal uptake or because the Indian Constitution recognizes it.',
    'If communal authority is genuinely pre-constitutional and rooted in living custom (not just the 1936 Act), the constraint is grounded in a durable authority structure (authority_grounding=practice or lineage). If authority derives from the 1936 Act and constitutional recognition, the constraint''s authority is contingent on the constitutional framework (authority_grounding=distributed or extraction). The reading_relations and cs_structure.axioms would shift in how they characterize the relationship to the secular_civil_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_vs_individual_authority_source, conceptual, 'Whether Parsi marriage authority is rooted in ancestral custom or in modern constitutional codification.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.42) primarily structural (Parsi individuals are barred by communal enforcement and face property/identity costs if they violate endogamy rules) or internalized (Parsi individuals have accepted endogamy as part of their identity and experience it as a self-evident constraint rather than an imposition)?',
    'Qualitative research with Parsis who have exited the community (civil marriage to non-Parsis, LGBTQ partnerships) examining their post-exit suppression trajectory: do they continue to experience suppression after the structural barriers are removed (internalized), or does suppression dissipate (structural). Comparative analysis with Parsis who remain within the community and experience no conflict (indicating strong internalization) vs. those who feel constant tension (indicating perceived structural imposition).',
    'If suppression is primarily internalized, the effective suppression for targets (interfaith couples, LGBTQ Parsis) is higher than the 0.42 structural measure suggests — the constraint carries the suppression internally, even after legal exit. The constraint''s effective extraction would be higher. If primarily structural, remedies that remove the structural barriers (legal recognition of interfaith marriage) would reduce suppression; if internalized, legal remedies alone would be insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the suppression enforcing endogamy is structural or internalized.').

omega_variable(
    false_summit_mountain_candidate,
    'Is the claim that Parsi communal custom is the ''natural'' or ''inevitable'' authority for Parsi marriage a false-summit mountain — a constructed constraint whose beneficiaries (parsi_community_collective, parsi_fire_temples) have declared it a natural cultural/religious fact, when it is actually a choice institutional structure with identifiable beneficiaries?',
    'Historical analysis of whether Parsi personal law existed before 1936 or was constructed by the 1936 codification; examination of whether there are Parsi communities or diaspora populations that have organized marriage and family law differently (e.g., civil law, secular community governance); analysis of what would happen if the 1936 Act were repealed — would communal custom spontaneously regenerate, or would Parsis default to civil law.',
    'If the constraint is a false-summit mountain (declared natural but constructed), the false_summit_mountain signature would fire and reclassify the constraint to its override target (default: tangled_rope, which is already the claim here, so no reclassification). If the constraint is genuinely rooted in pre-constitutional Parsi custom and would persist without the 1936 Act, the mountain claim is false and the classification stays as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, empirical, 'Whether Parsi communal authority is a natural cultural/religious fact or a constructed institutional choice.').

omega_variable(
    constitutional_pluralism_tension,
    'Can constitutional pluralism (recognizing multiple sources of family law authority: communal, religious, secular) coexist with constitutional non-discrimination (Articles 14, 15, 21: equality before law, non-discrimination on grounds of caste/sex/religion, right to life and personal liberty)? When endogamy enforcement under the Parsi reading violates the right to marry whom one chooses (an individual liberty), does constitutional pluralism trump individual rights, or vice versa?',
    'Constitutional analysis of whether Part III (fundamental rights) or the pluralist recognition in Article 25-28 (religious freedom and personal law pluralism) takes precedence; examination of how Indian courts have resolved conflicts between personal law and individual constitutional rights (e.g., Shah Bano case, recent same-sex marriage litigations); legislative history of the Constitution''s framing of the personal-law pluralism compromise.',
    'If individual constitutional rights take precedence (right to marry whom one chooses), endogamy enforcement becomes unconstitutional and the constraint loses its authority grounding; the reading collapses into the secular_civil_reading. If constitutional pluralism takes precedence, the Parsi reading retains legitimacy as a recognized minority law system, and the constraint persists. The answer determines whether this reading coexists_with the secular_civil_reading or forecloses it (likely coexists_with, since both readings are live in contemporary Indian law, but the empirical answer would clarify the logical structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_pluralism_tension, conceptual, 'Whether constitutional pluralism and constitutional non-discrimination can coexist when personal law rules conflict with individual rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.08).
narrative_ontology:measurement_basis(marr_tr_t1936, projected).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1960, observed).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1985, 0.13).
narrative_ontology:measurement_basis(marr_tr_t1985, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement_basis(marr_tr_t2015, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(marr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.22).
narrative_ontology:measurement_basis(marr_be_t1936, projected).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1960, 0.26).
narrative_ontology:measurement_basis(marr_be_t1960, observed).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement_basis(marr_be_t1985, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement_basis(marr_be_t2015, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(marr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.28).
narrative_ontology:measurement_basis(marr_su_t1936, projected).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement_basis(marr_su_t1960, observed).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1985, 0.37).
narrative_ontology:measurement_basis(marr_su_t1985, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement_basis(marr_su_t2015, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(marr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel. All five readings (parsi_communal, hindu_codified, muslim_shariat, christian_canonical, secular_civil) instantiate the same persisting kernel — a contested commitment about what source(s) of authority legitimize marriage and family law in India's pluralist constitutional system. Each reading grounds authority differently (communal custom, codified law + civil courts, Shariat + personal law boards, canonical law + church, constitutional individual rights + civil courts) and thus produces different constraints with different beneficiaries, victims, and extraction profiles. The five constraints are linked by network.affects_constraints as a constraint family: each reading's persistence or change affects the others' viability (one reading's legal victory constrains another's scope; one community's legal precedent affects others' expectations). The ε-invariance principle requires separate stories: each reading has a distinct constraint kernel and thus a distinct ε value and beneficiary/victim structure. The claim/metric divergence is per-reading: the Parsi communal reading claims tangled-rope and authors moderate extraction + rising suppression (the reading itself structures things this way); the secular_civil reading claims rope and would author lower extraction with lower suppression (that claim is a different story). These are not perspectives on one constraint; they are different constraints rooted in different authority sources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
