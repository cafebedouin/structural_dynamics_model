% ============================================================================
% CONSTRAINT STORY: parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parsi_zoroastrian_reading, []).

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
 *   constraint_id: parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law: Community Preservation Through Endogamy
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The Parsi Zoroastrian community in India (and diaspora) maintains strict
 *   endogamy requirements as a condition of religious membership and
 *   community standing. Marriage outside the community triggers automatic
 *   excommunication: loss of fire temple access, exclusion from community
 *   rituals, denial of burial rights in Parsi funerary grounds (Towers of
 *   Silence), and social ostracism. The enforcement mechanism is
 *   institutionally distributed across Panchayats (community councils),
 *   priestly authorities (Dasturs), and trust bodies (Bombay Parsi Punchayet,
 *   Zoroastrian Trust Funds Estate). This reading frames the constraint as
 *   one interpretation of contested family law authority — a lineage-grounded
 *   commitment system parallel to Hindu Dharmashastra, Muslim Shariat, and
 *   Christian canonical marriage law, but with distinctive ethnoreligious
 *   boundary logic. The constraint exhibits both genuine coordination
 *   function (preserving a small diaspora community in a post-colonial
 *   majority context where Parsis number fewer than 60,000 in India) and
 *   asymmetric extraction (disproportionate burden on women, children of
 *   mixed marriages, and would-be converts). Theater ratio (0.35) reflects
 *   increasing performativity: official enforcement persists while actual
 *   verification has atrophied, and diaspora communities increasingly
 *   disregard the rules without formal repudiation. Extractiveness (0.58) and
 *   suppression (0.70) have risen over the interval as demographic pressure
 *   intensifies the community's boundary anxiety and enforcement becomes more
 *   rigid despite declining functional capacity.
 *
 * KEY AGENTS:
 *   - Intermarried Individuals (especially women): Primary victims (powerless/identity_locked) — automatic excommunication with no biographical exit path; identity constituted through Zoroastrian belonging
 *   - Children of Mixed Marriages: Secondary victims (powerless/trapped) — bear costs they did not create; excluded from community despite Zoroastrian parent
 *   - Endogamous Family Heads: Mixed position (moderate/constrained) — benefit from community status and marriage market control but bear enforcement burden and social pressure
 *   - Priestly Authority (Dastur): Primary beneficiaries (institutional/arbitrage) — capture adjudication rents, maintain ritual monopoly, can grant exceptions or switch jurisdictions
 *   - Community Boundary Apparatus (Panchayats, Trust Bodies): Institutional beneficiaries (institutional/arbitrage) — maintain authority through boundary enforcement theater
 *   - Reform Coalition (ZTFE, Interfaith Advocates): Organized agents (organized/mobile) — building exit pathways through legal challenges, diaspora rule liberalization, demographic pressure narratives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both real coordination function (small-community preservation) and real asymmetric extraction (identity lock, gender asymmetry, children's exclusion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parsi_zoroastrian_reading, 0.58).
domain_priors:suppression_score(parsi_zoroastrian_reading, 0.7).
domain_priors:theater_ratio(parsi_zoroastrian_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parsi_zoroastrian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(parsi_zoroastrian_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(parsi_zoroastrian_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law: Community Preservation Through Endogamy").
narrative_ontology:topic_domain(parsi_zoroastrian_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parsi_zoroastrian_reading, '2c7a93e3-6cb6-411d-a0b7-552351421e7f').
narrative_ontology:cs_kernel_codification('2c7a93e3-6cb6-411d-a0b7-552351421e7f', formalized).
narrative_ontology:cs_authority_grounding('2c7a93e3-6cb6-411d-a0b7-552351421e7f', lineage).
narrative_ontology:cs_interpretation_layer_present('2c7a93e3-6cb6-411d-a0b7-552351421e7f').
narrative_ontology:cs_reading_relation('2c7a93e3-6cb6-411d-a0b7-552351421e7f', parsi_zoroastrian_reading__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c7a93e3-6cb6-411d-a0b7-552351421e7f', parsi_zoroastrian_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c7a93e3-6cb6-411d-a0b7-552351421e7f', parsi_zoroastrian_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c7a93e3-6cb6-411d-a0b7-552351421e7f', parsi_zoroastrian_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('2c7a93e3-6cb6-411d-a0b7-552351421e7f', foundational, zoroastrian_identity_by_birth_only).
narrative_ontology:cs_axiom_status(zoroastrian_identity_by_birth_only, holdable).
narrative_ontology:cs_axiom_grounding('2c7a93e3-6cb6-411d-a0b7-552351421e7f', zoroastrian_identity_by_birth_only, theological).
narrative_ontology:cs_axiom('2c7a93e3-6cb6-411d-a0b7-552351421e7f', foundational, ritual_efficacy_requires_lineage_purity).
narrative_ontology:cs_axiom_status(ritual_efficacy_requires_lineage_purity, holdable).
narrative_ontology:cs_axiom_grounding('2c7a93e3-6cb6-411d-a0b7-552351421e7f', ritual_efficacy_requires_lineage_purity, theological).
narrative_ontology:cs_axiom('2c7a93e3-6cb6-411d-a0b7-552351421e7f', secondary, community_survival_requires_endogamous_closure).
narrative_ontology:cs_axiom_status(community_survival_requires_endogamous_closure, holdable).
narrative_ontology:cs_axiom_grounding('2c7a93e3-6cb6-411d-a0b7-552351421e7f', community_survival_requires_endogamous_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('2c7a93e3-6cb6-411d-a0b7-552351421e7f', avestan_ritual_purity_doctrine).
narrative_ontology:cs_drift_state('2c7a93e3-6cb6-411d-a0b7-552351421e7f', contemporary_demographic_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c7a93e3-6cb6-411d-a0b7-552351421e7f', '2025-01-09T00:00:00Z').
narrative_ontology:cs_kernel_id(parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parsi_zoroastrian_reading, priestly_authority_structure).
narrative_ontology:constraint_beneficiary(parsi_zoroastrian_reading, endogamous_family_networks).
narrative_ontology:constraint_beneficiary(parsi_zoroastrian_reading, community_boundary_maintenance_apparatus).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, intermarried_individuals).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, children_of_mixed_marriages).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, women_seeking_conversion_entry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(parsi_zoroastrian_reading, endogamous_family_head).
narrative_ontology:constraint_beneficiary(parsi_zoroastrian_reading, priestly_authority_dastur).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, intermarried_woman).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, child_of_mixed_marriage).
narrative_ontology:constraint_victim(parsi_zoroastrian_reading, endogamous_family_head).
narrative_ontology:constraint_vindicates(parsi_zoroastrian_reading, blood_purity_preserves_faith).
narrative_ontology:constraint_vindicates(parsi_zoroastrian_reading, ritual_efficacy_requires_lineage).
narrative_ontology:constraint_vindicates(parsi_zoroastrian_reading, small_community_survival_requires_closure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born into Parsi Zoroastrian community, raised with Zoroastrian identity and fire temple participation. Marries outside the community (to a non-Zoroastrian). Automatic excommunication follows: loses fire temple access, excluded from navjote ceremonies for any children, denied burial rights in Towers of Silence, faces social ostracism from extended family and community networks. Identity is constituted through Zoroastrian belonging — physically could leave India or join another religious community, but biographical identity is fused with Parsi community membership. Exit is psychologically unthinkable from within her identity frame despite being structurally possible.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, intermarried_woman, payer,
    powerless, biographical, identity_locked, regional).

% Child with one Zoroastrian parent and one non-Zoroastrian parent. Raised with Zoroastrian cultural knowledge and religious practice by the Zoroastrian parent. Categorically excluded from navjote ceremony (initiation ritual), fire temple access, and community recognition regardless of upbringing or religious commitment. Bears costs created by parents' marriage decision with no agency in the original choice. Cannot exit because there is no exit — was never granted entry in the first place. Trapped in liminal status: culturally Zoroastrian but legally/religiously excluded.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, child_of_mixed_marriage, payer,
    powerless, biographical, trapped, regional).

% Head of endogamous Parsi family with adult children approaching marriage age. Benefits from community status, business networks within Parsi community, and marriage market control (endogamy reduces search costs, maintains property within community networks). Simultaneously bears enforcement burden: must police children's romantic relationships, faces social pressure to prevent intermarriage, risks status loss if children marry outside community. Constrained by social pressure and identity investment but has more agency than powerless agents — can choose to prioritize children's autonomy over community standing, can exit to diaspora where rules are more liberal.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, endogamous_family_head, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(parsi_zoroastrian_reading, endogamous_family_head, payer).

% Ordained Zoroastrian priest (Dastur) with authority over ritual validity and community standing determinations. Adjudicates marriage disputes, grants or denies navjote ceremonies, interprets endogamy rules. Captures institutional rents through adjudication fees, ritual monopoly, and discretionary exception-granting. Holds arbitrage exit: can interpret rules liberally or conservatively depending on which Panchayat jurisdiction offers more institutional support; can switch between regional Anjumans (Parsi community associations) if local enforcement becomes contested. Sets the agenda by determining which marriages and initiations are valid.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, priestly_authority_dastur, agenda_setter,
    institutional, immediate, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(parsi_zoroastrian_reading, priestly_authority_dastur, beneficiary).

% Traditional community councils (Panchayats) and trust bodies (Bombay Parsi Punchayet, Zoroastrian Trust Funds Estate) that maintain endogamy enforcement apparatus. Control access to community resources (trust funds, fire temples, burial grounds) and issue excommunication decrees. Institutional survival depends on boundary maintenance function — losing gatekeeping authority would eliminate the apparatus's reason for existence. However, actual enforcement capacity has degraded: no genetic verification, inconsistent record-keeping across regions, widespread diaspora non-compliance. Maintains enforcement theater while functional verification atrophies. Arbitrage exit: can shift enforcement rigor based on demographic pressure or legal challenges.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, panchayat_boundary_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Coalition of reformist Parsi organizations (Zoroastrian Trust Funds Estate reformist faction, interfaith marriage support groups, liberal Parsi associations) advocating for rule liberalization. Organized advocacy: legal challenges in Bombay High Court over trust fund access for children of mixed marriages, public campaigns documenting demographic collapse (population <60,000, intermarriage >30%, skewed sex ratios). Sees endogamy enforcement as transitional problem: demographic pressure will force rule change regardless of theological commitments. Mobile exit options: can point to diaspora communities (North America) that have already liberalized, can frame rule change as community survival strategy rather than theological betrayal. Excluded from traditional Panchayat decision-making but building parallel legitimacy structures.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, reform_coalition_ztfe, excluded,
    organized, generational, mobile, national).

% Academic or comparative law scholar analyzing the constraint from outside any single community's perspective. Sees both the genuine coordination function (small diaspora community preservation in post-colonial India where Parsis are <0.005% of population) and the asymmetric extraction (identity lock on biographical timescales, gender-asymmetric enforcement, children bearing costs they did not create, priestly rent capture). Not embedded in any stakeholder position; observes the structural gap between different agents' experiences without being subject to the constraint's direct effects.
narrative_ontology:constraint_stakeholder(parsi_zoroastrian_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates small-community preservation in a minority diaspora context: maintaining Parsi Zoroastrian demographic and cultural continuity in post-colonial India where the community numbers fewer than 60,000 and faces existential demographic pressures (aging population, skewed sex ratios, high intermarriage rates). Endogamy reduces within-community coordination costs (marriage matching, property transmission, ritual knowledge transfer) and maintains boundary clarity in a pluralistic legal system where religious personal law governs family matters.
% TRANSFER_FUNCTION: The constraint transfers institutional authority and social status from intermarried individuals (especially women) and their children to priestly authorities and endogamous family networks. Specifically: (1) Excommunication transfers fire temple access rights, burial rights, and community social capital from intermarried individuals to those who remain endogamous. (2) Priestly adjudication of marriage validity transfers decision-making authority and ritual fees from families to Dasturs. (3) Panchayat boundary maintenance transfers trust fund access and community resource control from mixed families to the institutional apparatus. The primary transfer is IDENTITY and BELONGING — from those who marry outside to those who police the boundary.
% ABSENT_VOICES: Would-be converts (especially women married to Parsi men who seek entry to the community for themselves and their children) are categorically excluded from the conversation — Zoroastrianism by birth only. Reform-minded younger Parsis who have already intermarried are excommunicated and thus removed from Panchayat deliberations despite being the demographic cohort most affected by the rule. Diaspora Parsis in communities that have liberalized (North America, UK reformist factions) are structurally outside Indian Panchayat jurisdiction and thus absent from the orthodox enforcement apparatus's decision-making. These absent voices would object that the endogamy requirement is demographic suicide disguised as theological necessity — but they are not in the room because the rule itself excludes them from having standing.
% DISAPPEARANCE_RATIONALE: If the endogamy requirement disappeared overnight, ARRANGEMENTS WOULD REARRANGE: (1) Intermarried individuals and their children would seek re-entry to fire temples and community institutions, forcing Panchayats to develop new gatekeeping criteria or abandon gatekeeping entirely. (2) Priestly authorities would lose adjudication rents and ritual monopoly over marriage validity. (3) Marriage market dynamics would shift as Parsi families could no longer rely on endogamy rule to constrain children's choices. (4) Trust fund governance and resource allocation would face immediate legal challenges from previously excluded mixed families. (5) Community boundary definition would require renegotiation — if not birth, then what criterion defines Zoroastrian belonging? The world rearranges because the constraint is load-bearing for institutional arrangements, not because it reflects a natural fact about community preservation. Contrast with a genuine natural law (gravity, arithmetic) where disappearance is incoherent — this constraint's disappearance is imaginable and would trigger concrete institutional and social reorganization.
% FOUNDING_PROBLEM: Historical founding problem (mid-to-late 19th century): Parsi community in British India faced demographic and cultural pressure from majority Hindu and Muslim populations, combined with British colonial legal codification of religious personal law. The endogamy requirement was formalized during this period as a boundary maintenance mechanism to prevent assimilation and preserve distinct ethnoreligious identity. Pre-colonial Parsi communities in India had more fluid boundaries and greater accommodation of intermarriage; the rigid endogamy rule emerged as a response to perceived existential threat in the colonial legal environment where religious communities were legally codified and competition for institutional resources intensified.
% FOUNDING_PROBLEM_CORROBORATION: The status is contested between two camps: (1) Orthodox Panchayats and traditionalist Parsi associations assert the founding problem (assimilation threat, demographic preservation) remains LIVE and has intensified due to declining population and high intermarriage rates — they cite demographic data showing population decline from 114,000 (1940s) to <60,000 (2020s) as evidence the threat persists. (2) Reform coalitions and diaspora communities assert the founding problem is DEAD or inverted — the endogamy rule itself is now the existential threat because it excludes children of mixed marriages who could otherwise sustain the community, and it drives younger Parsis away from community participation. Corroboration sources: (A) Orthodox position corroborated by Bombay Parsi Punchayet trustee statements, traditional Dastur pronouncements, demographic studies commissioned by conservative factions. (B) Reformist position corroborated by interfaith marriage advocacy groups, diaspora community surveys (North American Zoroastrian associations reporting acceptance of mixed marriages), legal scholars analyzing personal law reform (arguing demographic collapse justifies state intervention). No source outside the beneficiary/victim binary provides disinterested corroboration — the question is load-bearing for institutional survival and thus every source is embedded in the dispute.
narrative_ontology:disappearance_verdict(parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERMARRIED WOMAN (SNARE) — Identity-locked exit: structurally could leave the community but identity is constituted through Zoroastrian belonging; intermarriage triggers automatic excommunication with no appeal. Loses access to fire temples, community functions, burial rights, and social identity accumulated over biographical time. The coordination story (community preservation) is cover for exclusionary extraction — the constraint exists to maintain boundaries, not to solve a coordination problem the excluded agent created.
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENDOGAMOUS FAMILY HEAD (TANGLED ROPE) — Constrained by social pressure and identity investment but benefits from community status and marriage market control. Experiences both coordination (marriage matching within closed network reduces search costs) and extraction (must police children's choices to maintain standing). The constraint coordinates AND extracts — real coordination function (preserving small-community ties) embedded in asymmetric cost structure (enforcement burden falls on families, benefit accrues to priestly authority and boundary apparatus).
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIESTLY AUTHORITY (ROPE) — Arbitrage exit: can interpret rules, grant exceptions, or switch jurisdictions between regional Anjumans. Experiences the constraint as coordination mechanism: endogamy requirement channels marriage disputes to priestly adjudication, maintains ritual authority, and stabilizes the knowledge transmission chain. Net beneficiary — extraction flows toward priestly authority through adjudication fees, ritual monopoly, and institutional standing.
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents (Zoroastrian Trust Funds Estate, interfaith marriage support groups, liberal Parsi associations) see endogamy enforcement as transitional problem with implied sunset: demographic collapse (sex ratio imbalance, intermarriage rates >30%, population decline) is forcing rule liberalization. Legal challenges in Indian courts (Bombay High Court jurisdiction) and diaspora community splits (North American acceptance of children of mixed marriages) represent exit pathways. Experiences low effective extraction because coalition has agency and sees structural exit emerging through demographic pressure.
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PANCHAYAT RITUAL APPARATUS (PITON) — Traditional community councils (Panchayats) maintain enforcement theater but actual verification of endogamy has atrophied: no genetic testing, inconsistent record-keeping across regions, widespread unofficial acceptance of mixed marriages in diaspora. Ritual exclusion persists through institutional inertia and boundary-maintenance performance rather than functional community preservation. The apparatus sees its own process as degraded — maintained because alternative identity frameworks haven't fully replaced it, not because it prevents community dissolution.
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits both genuine coordination function (small diaspora community preservation in post-colonial India and diaspora contexts) AND asymmetric extraction (excommunication mechanism targets women disproportionately, children bear costs they did not create, priestly authority captures adjudication rents). The coordination function is REAL — Parsi community survival in minority context requires boundary maintenance — but the extraction is ALSO real and operates through identity lock on biographical timescales. Not a false summit: the demographic coordination problem exists independently of the extraction mechanism.
constraint_indexing:constraint_classification(parsi_zoroastrian_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parsi_zoroastrian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parsi_zoroastrian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parsi_zoroastrian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parsi_zoroastrian_reading, TR),
    TR >= 0.70.

:- end_tests(parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The constraint extracts biographical costs from intermarried individuals (especially women, who face automatic excommunication while men historically faced more lenient treatment) and imposes generational costs on children of mixed marriages who are excluded despite having a Zoroastrian parent. Priestly authorities and boundary-maintenance apparatus capture institutional rents through adjudication, exception-granting discretion, and ritual monopoly. The extraction is NOT total (0.72+) because the coordination function is real — small diaspora communities do face genuine survival pressures, and endogamy does reduce within-community coordination costs — but the asymmetry is significant enough that the constraint cannot be classified as pure coordination. Suppression (0.70): High. Exit options are severely constrained: intermarriage triggers automatic identity loss (no appeal mechanism in orthodox Panchayats), children cannot enter the community regardless of upbringing, and women seeking conversion are categorically excluded (Zoroastrianism by birth only). Legal barriers in Indian family law system reinforce rather than moderate the constraint. However, suppression is not total (0.85+) because diaspora exit pathways exist (North American communities liberalizing), and demographic collapse is forcing incremental rule changes. Theater ratio (0.35): Moderate and rising. Official enforcement persists (excommunication pronouncements, trust fund exclusions, fire temple access denial) but actual verification capacity has degraded: no genetic testing, inconsistent record-keeping across regional Panchayats, widespread diaspora non-compliance without formal penalty. The ritual apparatus maintains boundary-enforcement performance while the functional gatekeeping has weakened. Theater has increased from 0.20 (1850, robust enforcement) to 0.35 (2000, performative maintenance) as community size declined and diaspora fragmentation accelerated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates indexical classification across five DR types from the same structural base. The intermarried woman sees pure extraction (Snare) — the coordination story is cover; she is excluded to maintain boundaries she did not threaten before the rule was applied to her. The endogamous family head sees mixed coordination and extraction (Tangled Rope) — real community preservation benefits embedded in asymmetric enforcement burden. The priestly authority sees coordination mechanism (Rope) — endogamy channels marriage disputes to priestly adjudication and stabilizes transmission chains. The reform coalition sees temporary problem with sunset (Scaffold) — demographic collapse is forcing liberalization regardless of theological commitments. The Panchayat apparatus sees degraded ritual (Piton) — enforcement theater persists while functional verification has atrophied. The analytical observer sees tangled_rope at civilizational scale — BOTH coordination function (small diaspora survival) AND asymmetric extraction (identity lock, gender bias, children's exclusion) are structurally real. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' — and the gap reveals that the constraint operates differently for different agents in ways that cannot be resolved to a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the constraint. Intermarried women are FULL VICTIMS (d → 1.0): they bear maximum extraction (excommunication, identity loss, social death) with identity-locked exit (could physically leave but identity is constituted through Zoroastrian belonging). The engine derives high d from victim status + identity_locked exit → high f(d) → high χ. Endogamous family heads are MIXED (d → 0.5-0.6): they benefit from community status and marriage network control but also bear enforcement costs (policing children's choices, maintaining boundary surveillance). The engine derives moderate d from partial beneficiary + constrained exit. Priestly authorities are NET BENEFICIARIES (d → 0.2-0.3): they capture adjudication rents, maintain ritual monopoly, and hold arbitrage exit options (can grant exceptions, switch jurisdictions between Panchayats). The engine derives low d from beneficiary status + arbitrage exit → negative or low χ. The reform coalition experiences LOW EXTRACTION (d → 0.3-0.4) because they have agency (organized power), see structural exit paths (legal challenges, diaspora liberalization, demographic pressure), and frame the constraint as temporary (scaffold with implied sunset). The analytical observer computes MODERATE d (0.5) reflecting that the constraint exhibits BOTH coordination (real small-community preservation function) and extraction (asymmetric costs, identity lock, priestly rent capture) — this is why the analytical classification is tangled_rope rather than snare or rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through structural honesty: it is BOTH a coordination mechanism (preserving a small diaspora community in a post-colonial majority context) AND an extraction mechanism (excommunication targets women disproportionately, children bear costs they did not create, priestly authorities capture institutional rents). The analytical classification is tangled_rope because BOTH functions are real and load-bearing. This is NOT a false summit — the demographic coordination problem exists independently of the priestly extraction layer. The constraint would still face legitimacy challenges even if priestly rent-seeking were eliminated, because the identity-lock mechanism (automatic excommunication for intermarriage) imposes biographical costs that are difficult to justify when the community's survival is already in question. The mandatrophy resolution is that coordination and extraction are not mutually exclusive — a constraint can coordinate a real collective action problem (small-community preservation) while simultaneously extracting asymmetric costs from those least able to bear them (intermarried women, children of mixed marriages). The engine measures both functions independently and classifies based on their relative magnitudes across different perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Parsi Zoroastrian endogamy requirement a reading of contested family law authority, or a distinct ethnoreligious preservation constraint orthogonal to the marriage authority kernel?',
    'Historical analysis: Does the constraint''s structure derive from theological marriage doctrine (parallel to Hindu/Muslim/Christian readings) or from post-colonial minority survival logic? Do Parsi religious authorities cite Avestan marriage theology or demographic preservation when defending endogamy?',
    'If theological: this constraint is a legitimate kernel reading alongside Hindu Dharmashastra and Muslim Shariat. If demographic: the constraint is outside the family law authority kernel and should be reframed as ethnoreligious boundary maintenance (distinct constraint family).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this constraint is a kernel reading or orthogonal ethnoreligious logic').

omega_variable(
    demographic_threshold_for_rule_change,
    'At what population threshold does demographic collapse force endogamy rule liberalization despite theological commitments?',
    'Cross-diaspora comparison: Parsi populations in India (declining, rules rigid), North America (stable, rules liberalizing), UK (mixed, rules contested). Identify the population size, sex ratio, or intermarriage rate at which communities abandon enforcement regardless of theological position.',
    'If threshold already crossed in India (population <60,000, intermarriage >30%): scaffold sunset is structural and imminent. If threshold not yet reached: tangled rope persists for another generation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_threshold_for_rule_change, empirical, 'Population threshold triggering rule liberalization regardless of theology').

omega_variable(
    gender_asymmetry_enforcement,
    'Is the disproportionate enforcement against women (automatic excommunication for intermarriage) a feature of the theological reading or a contingent patriarchal overlay?',
    'Avestan textual analysis: Do Zoroastrian scriptures mandate gender-asymmetric marriage rules, or is the asymmetry a product of Parsi community interpretation in patriarchal Indian legal context? Compare with Iranian Zoroastrian communities (different gender enforcement patterns) and historical Parsi practice pre-1900.',
    'If theological: the gender asymmetry is load-bearing to this reading''s identity (axiom-level commitment). If contingent: the asymmetry is extractive overlay on a gender-neutral theological core, and removing it would not dissolve the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_enforcement, empirical, 'Whether gender-asymmetric enforcement is theologically mandated or contingent').

omega_variable(
    priestly_interpretation_authority_scope,
    'Do individual priests (Dasturs) have canonical authority to grant marriage exceptions, or is their discretion itself a post-migration innovation without Avestan grounding?',
    'Comparative analysis: Iranian Zoroastrian priestly authority structures vs Indian Parsi Panchayat system. Historical record of priestly exception-granting before and after British colonial legal codification. Avestan canon on priestly interpretive authority.',
    'If canonical: priestly arbitrage is theologically grounded, and the institutional/arbitrage perspective is structurally legitimate. If innovation: priestly authority is itself an extractive layer, and the institutional perspective''s ''rope'' classification is suspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_interpretation_authority_scope, empirical, 'Whether priestly exception-granting authority is canonically grounded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parsi_zoroastrian_reading, 0, 175).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_theater_1850, parsi_zoroastrian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(parsi_theater_1900, parsi_zoroastrian_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(parsi_theater_1950, parsi_zoroastrian_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(parsi_theater_2000, parsi_zoroastrian_reading, theater_ratio, 150, 0.35).

% Extraction over time
narrative_ontology:measurement(parsi_extract_1850, parsi_zoroastrian_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(parsi_extract_1900, parsi_zoroastrian_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(parsi_extract_1950, parsi_zoroastrian_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(parsi_extract_2000, parsi_zoroastrian_reading, base_extractiveness, 150, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(parsi_suppress_1850, parsi_zoroastrian_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(parsi_suppress_1900, parsi_zoroastrian_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(parsi_suppress_1950, parsi_zoroastrian_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(parsi_suppress_2000, parsi_zoroastrian_reading, suppression_requirement, 150, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The Parsi Zoroastrian endogamy constraint is part of the family_law_authority constraint family. Each reading (Hindu, Muslim, Christian, Parsi, Secular) has its own extractiveness value reflecting the specific institutional arrangements of that religious or legal tradition. The Parsi reading's high extractiveness (0.58) and suppression (0.70) reflect the combination of identity-lock mechanism, demographic anxiety, and priestly rent capture. Network edges represent structural influence: the Parsi reading's demographic collapse and resulting legal challenges (Bombay High Court jurisdiction over trust fund governance) create pressure on the secular reading to articulate limits on religious autonomy when community survival is at stake. The Parsi reading also influences Hindu and Muslim readings by providing a comparison case for how ethnoreligious minorities negotiate boundary maintenance in a pluralistic legal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
