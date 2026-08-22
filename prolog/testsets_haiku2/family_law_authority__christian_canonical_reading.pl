% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Marriage as Sacrament under Christian Ecclesiastical Authority
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the
 *   family_law_authority kernel: the Christian ecclesiastical reading that
 *   grounds marriage validity and dissolution authority in theological
 *   doctrine and church hierarchy. In the Christian canonical reading
 *   (Catholic most rigidly, Protestant denominations with variance), marriage
 *   is a sacrament whose permanence is spiritually mandated and whose
 *   validity is determined by ecclesiastical authority. This reading coexists
 *   with secular-contractual, Hindu dharmashastra, Muslim shariat, and
 *   Zoroastrian readings—each declaring different authorities (state law,
 *   Vedic texts, Quranic verses, Avestan tradition) as legitimate sources of
 *   marriage law. The SCOPE manifest's kernel decomposition directs
 *   generation of each reading as a separate constraint story, each with its
 *   own ε, beneficiaries, victims, and authority structure. This story
 *   reports the Christian reading's internal structure: who benefits from
 *   ecclesiastical authority over marriage, who bears costs, and what
 *   structural contradictions emerge as secular law and alternative
 *   theological readings challenge its monopoly on legitimacy.
 *
 * KEY AGENTS:
 *   - Ecclesiastical authority (Catholic hierarchy or Protestant denominational leadership): agenda-setter, defines marriage validity and permanence doctrine, controls annulment processes, enforces sacramental understanding through pastoral discipline (excommunication, exclusion from sacraments). Power: institutional. Exit: analytical (no exit from defining doctrinal authority; the role IS the authority structure).
 *   - Married couples within faith: identity-locked beneficiaries with secondary costs. Power: moderate individually, organized collectively. Exit: identity_locked (marital identity is fused with faith identity; divorce requires identity rupture).
 *   - Divorced persons: powerless payers, structurally excluded from decision-making. Power: powerless. Exit: trapped (remain defined by ecclesiastical law as 'divorced' regardless of civil status or remarriage).
 *   - Women in abusive or incompatible marriages: powerless payers seeking dissolution. Power: powerless. Exit: identity_locked (faith identity makes exit psychologically costly) and materially trapped (annulment processes favor wealthy, educated applicants with legal resources).
 *   - State civil law: excluded institutional actor. Power: institutional. Exit: constrained (in secular polities, state is forced to recognize civil divorce as superseding ecclesiastical prohibition; in theocratic contexts, state enforces ecclesiastical rule).
 *   - Faith community: organized beneficiary. Power: organized. Exit: analytical (community cohesion benefits from coherent marriage doctrine).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.68).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament under Christian Ecclesiastical Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '26466904-c10d-45e1-8e3a-4e878150d638').
narrative_ontology:cs_kernel_codification('26466904-c10d-45e1-8e3a-4e878150d638', fixed_text).
narrative_ontology:cs_authority_grounding('26466904-c10d-45e1-8e3a-4e878150d638', lineage).
narrative_ontology:cs_interpretation_layer_present('26466904-c10d-45e1-8e3a-4e878150d638').
narrative_ontology:cs_reading_relation('26466904-c10d-45e1-8e3a-4e878150d638', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_reading_relation('26466904-c10d-45e1-8e3a-4e878150d638', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('26466904-c10d-45e1-8e3a-4e878150d638', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('26466904-c10d-45e1-8e3a-4e878150d638', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('26466904-c10d-45e1-8e3a-4e878150d638', foundational, sacramental_indissolubility_doctrine).
narrative_ontology:cs_axiom_status(sacramental_indissolubility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('26466904-c10d-45e1-8e3a-4e878150d638', sacramental_indissolubility_doctrine, deontological).
narrative_ontology:cs_axiom('26466904-c10d-45e1-8e3a-4e878150d638', foundational, ecclesiastical_authority_over_conscience).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_conscience, holdable).
narrative_ontology:cs_axiom_grounding('26466904-c10d-45e1-8e3a-4e878150d638', ecclesiastical_authority_over_conscience, theological).
narrative_ontology:cs_reference_frame('26466904-c10d-45e1-8e3a-4e878150d638', apostolic_authority_lineage).
narrative_ontology:cs_drift_state('26466904-c10d-45e1-8e3a-4e878150d638', contemporary_pluralist_polities, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('26466904-c10d-45e1-8e3a-4e878150d638', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, married_couples_within_faith).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, faith_community).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_persons).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, mixed_faith_couples).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, women_seeking_marriage_dissolution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, married_couples_within_faith).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesiastical_jurisdiction_over_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, magisterial_authority_in_matters_of_conscience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Catholic Church (or Protestant denominational leadership) sets canonical law regarding marriage validity, determines impediments to marriage, administers the sacrament through authorized clergy, authorizes annulments, and enforces doctrine through pastoral discipline. Controls the interpretive apparatus: bishops, theologians, and canon lawyers who determine what counts as a valid marriage and whether an annulment is granted. Collects authority over intimate life decisions, collects legitimacy by controlling access to sacramental validation, collects institutional coherence by maintaining doctrinal consistency.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Receive sacramental blessing, community recognition, spiritual legitimacy of their marriage commitment, and pastoral support through the marriage and family life. Their marital status is publicly recognized and religiously validated. They also bear costs: commitment is framed as spiritually permanent (Catholic) or under strict ecclesiastical conditions (Protestant); seeking divorce is spiritually condemned and administratively blocked; remarriage without annulment is forbidden. Their identity as a faithful person is constituted through marital status; divorce would mean identity rupture or religious exile.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, married_couples_within_faith, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, married_couples_within_faith, payer).

% Carry the ecclesiastical definition of themselves as 'divorced' regardless of their material or legal status. Excluded from full participation in sacramental life (cannot receive Eucharist without confession and amendment, cannot remarry sacramentally). Subject to pastoral judgment, social stigma within faith community, and spiritual condemnation in conservative communities. Their attempt to exit the marriage constraint fails—they remain defined and disciplined by it. Remarriage in civil courts does not restore legitimacy within the faith. The constraint follows them into new relationships.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_persons, payer,
    powerless, biographical, trapped, local).

% Face compounded barriers to exit: ecclesiastical prohibition on divorce, identity-lock (faithful Catholic or Protestant women's self-concept is centered on wifehood and motherhood), administrative barriers (annulment requires proving impediment at time of marriage, not current harm; processes are expensive and available unevenly across dioceses). Abuse, infidelity, and incompatibility are not recognized grounds for annulment in strict Catholic practice. Even successful annulment retroactively erases the marriage, creating psychological and legal ambiguity about children born within the marriage. The constraint actively suppresses their exit while framing non-exit as a virtue (patience, sacrifice, fidelity).
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, women_seeking_marriage_dissolution, payer,
    powerless, biographical, identity_locked, local).

% One partner's marriage is recognized as sacramental by one tradition but not by the other's. Face administrative barriers (dispensations, promises about children's upbringing in the faith). Doctrinal friction: the non-Catholic or non-Protestant spouse's marriage is treated as incomplete or conditionally valid. The constraint's enforcement includes conditional recognition designed to reinforce ecclesiastical authority over the non-Catholic spouse. Divorce becomes administratively complex because two separate ecclesiastical authorities may not recognize each other's dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, mixed_faith_couples, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, mixed_faith_couples, excluded).

% Benefits from ecclesiastical control of marriage: the constraint stabilizes family formation around shared sacramental understanding, prevents rapid remarriage (which the faith reads as infidelity or abandonment of commitment), reinforces community cohesion through common norms and shared ritual. The constraint vindicates the faith's foundational claim to authority over conscience and intimate life—a crucial legitimacy stake for institutional religion in pluralist societies.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, faith_community, beneficiary,
    organized, generational, analytical, national).

% In secular polities, the state has enacted civil divorce law that contradicts ecclesiastical indissolubility doctrine. State law recognizes divorce, permits remarriage, and treats marriage as a civil contract terminable by consent or judicial decree. Ecclesiastical law treats divorce as invalid and remarriage as adulterous. The constraint's persistence depends on the state NOT enforcing its own civil remedies within faithful populations (or on state recognition of ecclesiastical annulments as valid substitutes for civil divorce). In theocratic or formally dualist contexts (where ecclesiastical law is formally recognized), state enforces the ecclesiastical rule. In secular polities, state exclusion is structural: the constraint can only persist to the extent the state permits parallel legal systems.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, state_civil_law, excluded,
    institutional, generational, constrained, national).

% The doctrinal claim that marriage is sacramental and (in Catholic reading) indissoluble is vindicated by the constraint's enforcement. This is not an actor but an abstraction—the theological tradition itself, as encoded in doctrinal texts and institutional memory. Doctrinal coherence is protected by the constraint's operation; alternative interpretations (that Jesus's teaching applied only to specific historical circumstances, or that mercy and human flourishing might justify remarriage) are suppressed through institutional authority and pastoral discipline.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, theological_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(family_law_authority__christian_canonical_reading, theological_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, theologically consistent framework for recognizing valid marriages, determining their permanence, and adjudicating dissolution claims. Coordinates family formation around a shared sacramental understanding, preventing fragmented recognition (one party's marriage might be invalid by another tradition's lights). Provides pastoral support and spiritual legitimacy for commitment.
% TRANSFER_FUNCTION: Moves authority over marriage validity and dissolution from individual conscience or civil law to ecclesiastical hierarchy. Transfers the right to define legitimate marriage, determine impediments, authorize remarriage, and impose spiritual consequences for violation from the couple to the church. In practical terms: transfers time (annulment processes, pastoral counseling), money (annulment fees, marriage preparation courses), and reproductive/domestic autonomy (decisions about divorce and remarriage are constrained by ecclesiastical doctrine).
% ABSENT_VOICES: Divorced persons are structurally excluded from the conversation that decides their status—they experience the constraint but are not seated as decision-makers. Women in abusive marriages, who seek dissolution but lack grounds recognized by canon law, are treated as absent from the legitimacy deliberation. Civil law authorities are excluded in theocratic or formally dualist contexts. LGBTQ+ persons seeking same-sex marriage were historically entirely excluded (now partially admitted in some Protestant denominations, but excluded in Catholic doctrine).
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over marriage validity disappeared overnight, family formation would reorganize around civil law, individual consent, and personal conscience. Remarriage after civil divorce would become routine and shame-free. Annulment processes would vanish or be replaced by civil procedures. Women in harmful marriages would have accessible exit. The faith community's coherence around marriage doctrine would fragment—different Christian traditions already interpret marriage permanence differently (Catholics forbid remarriage; many Protestants permit it; nearly all now accept remarriage in some circumstances). The constraint's disappearance would expose these contradictions, historically masked by institutional authority.
% FOUNDING_PROBLEM: Early Christian theology confronted Roman marriage law, which permitted both divorce and remarriage. The church claimed authority to define marriage as spiritually permanent, grounded in Jesus's teaching against divorce (Matthew 19, Mark 10) and Paul's writings on marriage as a 'sacrament' reflecting Christ-Church union (Ephesians 5). The founding problem was: How does the church establish its authority to override secular law and personal preference on the intimate matter of marriage dissolution?
% FOUNDING_PROBLEM_CORROBORATION: Catholic and some Protestant theologians attest the founding problem remains live: doctrinal permanence is still necessary to prevent moral disorder and protect the sacramental meaning. Divorced Catholics, remarried persons, civil law authorities, and scholars of scripture attest the founding problem has shifted—Jesus's teaching is read (by alternative exegesis) as addressing a specific historical circumstance, not as eternal doctrine; and the church's institutional authority is contested, not assumed. Sociological evidence from jurisdictions with civil divorce shows family dissolution does not produce the moral chaos the founding doctrine predicted, supporting the 'shifted function' reading.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (interval end, 2025) because the constraint transfers authority over intimate life decisions from individuals to ecclesiastical hierarchy, imposes spiritual penalties for non-compliance (excommunication, denial of sacraments), and prevents exit through remarriage. Historical extractiveness (t=1000) was higher (0.81) when church authority was uncontested and state alternatives did not exist; it declined through the Enlightenment and 19th-century secularization as civil divorce became available, but has re-stabilized in the modern era as denominational authority reconstructs itself in pluralist contexts (remarriage barriers have not diminished, just moved to governance within shrinking faithful populations). Theater ratio has risen from 0.15 (t=1000, when sacramental meaning was the primary function) to 0.41 (t=2025): a growing share of ecclesiastical activity is now defensive—maintaining the appearance of coherence and authority in the face of contradictory practices (remarriage in most Protestant denominations while claiming permanence; tolerating divorced-and-remarried Catholics in many parishes while formally maintaining indissolubility doctrine). Suppression has declined from 0.88 to 0.72 because legal alternatives (civil divorce, secular remarriage) are now available in most jurisdictions, reducing the constraint's coercive force—but within faithful populations, suppression remains high because identity-lock and spiritual penalties remain potent. Accessibility collapse is 0.65: alternative marriage frameworks (secular contractual, denominational variance) ARE visible and accessible to faithful persons, but identity-lock prevents most from choosing them; the collapse is internalized more than structural.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical authority seat computes this as genuine coordination (sacramental validation, community recognition, spiritual meaning) with legitimate pastoral enforcement. Married couples within the faith often experience it similarly—the sacramental framing is constitutive of their marital identity. Divorced persons and women seeking dissolution compute it as pure extraction: they have no access to the coordination benefits (remarriage is forbidden) and bear all the costs (stigma, spiritual exclusion, administrative barriers). The engine computes these divergent types from structural data: beneficiary seats (ecclesiastical authority, faith community) derive d toward 0.0 (full beneficiary), computing Rope or even coordination-dominant types; victim seats (divorced persons, women in harmful marriages) derive d toward 1.0 (full target), computing extraction-dominant types. The claim is Tangled Rope because the constraint genuinely coordinates family formation around shared sacramental meaning (real coordination function) while extracting authority from individuals to hierarchy and suppressing exit (real extraction). The seat divergence is the evidence of the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority: d ≈ 0.0 (full beneficiary). Sets the rules, controls the apparatus, collects institutional authority and legitimacy. No exit option (the role IS defining authority). Married couples: d ≈ 0.4 (moderate beneficiary with asymmetric costs). Receive sacramental validation and community recognition; bear identity-lock (exit is psychologically costly because marital identity fuses with faith identity) and spiritual penalties. Identity-locked exit raises their effective d toward the target end, but genuine coordination benefits (spiritual meaning, pastoral support) lower it. Divorced persons: d ≈ 0.9 (near-total target). No coordination benefit (remarriage is forbidden, so they cannot access the sacramental system). Trapped exit (remain ecclesiastically defined as 'divorced' regardless of civil status). Receive only costs: stigma, spiritual exclusion, administrative barriers. Women seeking dissolution: d ≈ 0.95 (near-total target, higher than divorced persons because they seek to exit and are actively suppressed). Trapped by both ecclesiastical rule and patriarchal doctrine that frames women's identity through marriage; identity-locked and material barriers (expense, length of annulment) operate together. State civil law: d ≈ 0.55 (near-symmetric, slightly toward target). The constraint extracts authority from state governance over marriage but, in secular polities, state law supersedes ecclesiastical law, creating an asymmetric institutional contest—state is prevented from enforcing its own rules where ecclesiastical authority is strong (immigrant communities, religiously cohesive regions). Directionality overrides are not necessary because the beneficiary/victim + exit derivation captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is: ecclesiastical authority establishes its right to define marriage as sacramental and permanent, overriding secular law and personal preference. This problem was LIVE when the church was the only marriage-legitimating authority (medieval Christendom). By the 18th-19th centuries, the problem was clearly shifting: civil law established competing authority, and divorce became routinely available in secular jurisdictions. By 2025, the founding problem is DEAD in most contexts: secular civil law is the default marriage framework, ecclesiastical authority is residual and denominationally fragmented (Catholics maintain indissolubility doctrine, most Protestants permit remarriage, Orthodox permits remarriage in narrower circumstances). Yet the constraint persists with high theater: the Catholic Church maintains indissolubility doctrine and annulment processes, not because the founding problem (establishing ecclesiastical authority) is live, but because the doctrine is now a marker of institutional identity and doctrinal coherence. The constraint is a Piton candidate: the foundational justification (preventing divorce, establishing church authority over intimate life) has atrophied as civil law supersedes it, but the constraint persists as theatrical maintenance of doctrinal purity. The rising theater ratio (0.15 to 0.41) is the smoking gun: ecclesiastical activity is increasingly devoted to maintaining the appearance of coherence (defending why Catholics cannot remarry while 50% of Catholics divorce anyway; why annulment is 'not the same as divorce' while serving the same function) rather than to the original coordination function (providing sacramental legitimacy for stable families). A true Rope would show theater declining as the coordination function proved itself; a true Snare would show theater near or below baseline (pure extraction, no pretense needed). Rising theater on a stagnant ε suggests institutional inertia masquerading as principled doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_suppression,
    'How much of the measured suppression is internalized (identity-locked faithful persons accepting ecclesiastical authority as legitimate) versus structural (external barriers like legal unavailability of divorce or ecclesiastical exclusion)?',
    'Post-exit trajectory analysis: if Catholic divorcees maintain suppression (internalized norms, shame, continued belief in indissolubility) after civil remarriage, suppression is partially internalized; if suppression drops after civil remarriage becomes sacramentally recognized or church approval is obtained, suppression was primarily structural. Compare cohorts: do younger Catholics raised in post-Vatican-II pluralism show lower internalized suppression than older cohorts? Do immigrant communities with lower civil-legal literacy show higher structural suppression?',
    'If suppression is highly internalized, the constraint''s effective suppression is stickier than the measured 0.72 suggests—exit requires not just legal change but identity reconstruction. If mostly structural, legal remedies (permitting remarriage, lowering annulment barriers) would decompress suppression rapidly. Affects classification: higher internalized proportion moves constraint toward Snare (extraction is self-enforced); higher structural proportion keeps it Tangled Rope (extraction requires active institutional maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'The composition of suppression: internalized identity-lock versus structural barriers.').

omega_variable(
    authority_foreclosure_vs_coexistence,
    'Does the Christian ecclesiastical reading logically foreclose the secular contractual reading (assert that only one can be true in any single framework), or do they coexist as different parties'' commitments?',
    'Examine doctrine: Catholic theology declares secular divorce invalid and remarriage adulterous—a direct truth-claim that contradicts secular law''s truth-claim that civil divorce terminates marriage. If both are asserted as universal truths applying to the same marriage, they foreclose each other. If each party accepts the other''s authority within its own domain (church recognizes civil divorce as dissolving civil status; state recognizes annulment as a church procedure within church), they coexist. Textual analysis of post-Vatican-II papal encyclicals clarifies whether the church claims its doctrine is binding on all persons or only on the faithful.',
    'Foreclosure would place the reading_relation as ''forecloses'' (rare, structural contradiction). Coexistence would place it as ''coexists_with'' (live positions held by different parties with no logical resolution). The choice affects how the engine models the kernel''s stability: foreclosure implies one reading must eventually dominate or one must recede; coexistence implies indefinite pluralism or ongoing institutional contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_foreclosure_vs_coexistence, conceptual, 'Whether Christian canonical and secular contractual readings logically foreclose each other or coexist as different parties'' claims.').

omega_variable(
    sacramental_necessity_vs_institutional_identity,
    'Is the measured extraction (0.68) a necessary cost of authentic sacramental marriage coordination, or is it institutional identity-capture (the church extracts authority partly because institutional survival depends on controlling this domain)?',
    'Comparative analysis: do non-Catholic Christian traditions that permit remarriage show lower ecclesiastical extraction while maintaining sacramental language? Do traditions that recognize civil divorce as dissolving the sacrament show different beneficiary/victim structures? Do regions where civil law is unambiguously supreme (secular states) show lower theater_ratio and lower suppression among the faithful (suggesting the extraction is institutional self-defense, not essential to coordination)?',
    'If sacramental necessity, high extraction is the price of genuine coordination—move toward Rope classification (coordination-weighted). If institutional identity-capture, extraction is overhead on a coordination function that could persist with lower authority concentration—move toward Snare classification (extraction-weighted). Affects mandatrophy analysis: genuine Rope may not be mandatrophic even with high extraction; Snare with rising theater is mandatrophic by definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_necessity_vs_institutional_identity, empirical, 'Whether extraction is structurally necessary for sacramental coordination or represents institutional self-preservation.').

omega_variable(
    reading_variance_within_christendom,
    'Should this constraint be decomposed further to separate Catholic from Protestant readings, or does treating ''Christian canonical reading'' as a family with internal variance capture the structure accurately?',
    'Parameter comparison: Catholic reading (indissolubility doctrine, no-remarriage rule) produces higher ε (~0.72) and higher suppression (~0.72) than most Protestant readings (remarriage permitted, higher annulment accessibility), which produce lower ε (~0.45) and lower suppression (~0.35). If the readings produce divergent classifications (Catholic = Snare, Protestant = Rope), they should be decomposed. If they remain Tangled Rope under both parameters, variance can be captured via omega rather than decomposition.',
    'Decomposition would create two constraint stories (catholic_canonical_reading, protestant_denominational_reading) linked by network.affects_constraints, each with its own ε and stakeholder structure. Keeping them unified treats doctrinal variance as internal rather than structural. Decision affects network topology and per-seat classification: a unified story''s Catholic seats compute differently from Protestant seats within the same constraint; decomposed stories keep each tradition''s internal logic clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_variance_within_christendom, conceptual, 'Whether Christian canonical variance is one constraint family with reading-specific omegas or multiple constraints requiring separate stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1000, family_law_authority__christian_canonical_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement_basis(fami_tr_t1000, projected).
narrative_ontology:measurement(fami_tr_t1500, family_law_authority__christian_canonical_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement_basis(fami_tr_t1500, projected).
narrative_ontology:measurement(fami_tr_t1750, family_law_authority__christian_canonical_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement_basis(fami_tr_t1750, projected).
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__christian_canonical_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(fami_tr_t1900, observed).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__christian_canonical_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement_basis(fami_tr_t1960, observed).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__christian_canonical_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(fami_tr_t2000, observed).
narrative_ontology:measurement(fami_tr_t2025, family_law_authority__christian_canonical_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(fami_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t1000, family_law_authority__christian_canonical_reading, base_extractiveness, 1000, 0.81).
narrative_ontology:measurement_basis(fami_be_t1000, projected).
narrative_ontology:measurement(fami_be_t1500, family_law_authority__christian_canonical_reading, base_extractiveness, 1500, 0.79).
narrative_ontology:measurement_basis(fami_be_t1500, projected).
narrative_ontology:measurement(fami_be_t1750, family_law_authority__christian_canonical_reading, base_extractiveness, 1750, 0.76).
narrative_ontology:measurement_basis(fami_be_t1750, projected).
narrative_ontology:measurement(fami_be_t1900, family_law_authority__christian_canonical_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement_basis(fami_be_t1900, observed).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__christian_canonical_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement_basis(fami_be_t1960, observed).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__christian_canonical_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(fami_be_t2000, observed).
narrative_ontology:measurement(fami_be_t2025, family_law_authority__christian_canonical_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(fami_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1000, family_law_authority__christian_canonical_reading, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement_basis(fami_su_t1000, projected).
narrative_ontology:measurement(fami_su_t1500, family_law_authority__christian_canonical_reading, suppression_requirement, 1500, 0.85).
narrative_ontology:measurement_basis(fami_su_t1500, projected).
narrative_ontology:measurement(fami_su_t1750, family_law_authority__christian_canonical_reading, suppression_requirement, 1750, 0.82).
narrative_ontology:measurement_basis(fami_su_t1750, projected).
narrative_ontology:measurement(fami_su_t1900, family_law_authority__christian_canonical_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement_basis(fami_su_t1900, observed).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__christian_canonical_reading, suppression_requirement, 1960, 0.72).
narrative_ontology:measurement_basis(fami_su_t1960, observed).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__christian_canonical_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(fami_su_t2000, observed).
narrative_ontology:measurement(fami_su_t2025, family_law_authority__christian_canonical_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(fami_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).

% DUAL FORMULATION NOTE:
% This story instantiates one reading of the family_law_authority kernel. The constraint's structure (beneficiaries, victims, suppression mechanisms) is specific to the ecclesiastical authority grounding declared in this reading. Sibling readings (secular_contractual, hindu_dharmashastra, muslim_shariat, parsi_zoroastrian) instantiate competing authorities over marriage validity and dissolution. Each reading produces a different ε because each measures extraction relative to its own declared authority framework: ecclesiastical extraction is measured against the costs/benefits of church-administered marriage coordination; secular extraction is measured against state-administered civil coordination. Shared referent (the standing marriage arrangement under contest) but reading-indexed ε values. The kernel's stability depends on whether these readings foreclose each other (only one can be true in any single framework) or coexist (different parties hold different readings simultaneously). The constraint family is linked by directed influence: Christian reading influences secular reading (religious authority claims feed into resistance to secular marriage law); secular reading influences Christian reading (state law's availability functions as an alternative exit, increasing suppression requirement within ecclesiastical domains). Network evidence: rising theater_ratio in Christian reading correlates with decades of state-law availability of divorce—the ecclesiastical institution must work harder to maintain coherence as the structural basis (monopoly on marriage authority) erodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
