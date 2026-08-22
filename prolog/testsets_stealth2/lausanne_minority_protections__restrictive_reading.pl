% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Restrictive Reading of the Lausanne Minority Protections (Individual Worship Only)
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   The restrictive reading of the 1923 Treaty of Lausanne holds that
 *   Articles 37-45 protect only the individual religious exercise of Turkey's
 *   non-Muslim minorities — worship, personal observance, narrowly scoped
 *   confessional schooling — while institutional autonomy, foundation
 *   property, and clergy formation fall outside the treaty and remain
 *   governed by ordinary Turkish law. Operationally this arrangement has
 *   meant denial of legal personality to the Ecumenical Patriarchate, closure
 *   of its theological academy since 1971, invalidation of foundation
 *   property acquired after the 1936 declarations, citizenship conditions on
 *   religious office, and a century of asset transfers executed through
 *   courts and the General Directorate of Foundations. CONSTRAINT-FAMILY
 *   NOTE: the colloquial label 'Lausanne minority protections' decomposes
 *   into three structurally distinct claims (epsilon-invariance
 *   decomposition). This story authors the RESTRICTIVE instantiation, whose
 *   epsilon (0.74) is indexed to the standing arrangement of institutional
 *   subordination as this reading constitutes it. The expansive sibling
 *   (lausanne_minority_protections__expansive_reading) reads the same
 *   articles as guaranteeing functional continuity of pre-1923 religious
 *   governance and would largely empty the victim set; the guarantor sibling
 *   (lausanne_minority_protections__guarantor_reading) leaves substantive
 *   scope aside and relocates enforcement to international supervision. The
 *   three readings share a referent and diverge in epsilon and victim
 *   structure, which is why they are separate files linked by
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   turkish_state_apparatus: agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — administers the general-law regime, captures
 *   forfeited assets, controls the interpretive premise -
 *   ecumenical_patriarchate: primary target (organized/identity_locked) —
 *   denied legal personality and clergy formation; exit would dissolve the
 *   see - greek_orthodox_foundations: primary target (moderate/trapped) —
 *   asset seizures under the declaration-invalidation mechanism -
 *   armenian_apostolic_institutions: target (moderate/trapped) -
 *   turkish_jewish_community: target (moderate/constrained) -
 *   orthodox_clergy_candidates: target (powerless/constrained) — educational
 *   foreclosure - istanbul_minority_residents: dual-positioned
 *   (powerless/constrained) — individual worship protected, institutional
 *   base eroded - muslim_majority_institutions: secondary beneficiary
 *   (institutional/mobile) — relative advantage under the same regime -
 *   hellenic_republic: excluded advocate (institutional/arbitrage) — outside
 *   the domestic interpretive conversation - european_human_rights_bodies:
 *   analytical observer (institutional/analytical)
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: agenda-setter and primary beneficiary (institutional/arbitrage) — administers the general-law regime, captures forfeited assets, controls the interpretive premise
 *   - ecumenical_patriarchate: primary target (organized/identity_locked) — denied legal personality and clergy formation; relocation would dissolve the see
 *   - greek_orthodox_foundations: primary target (moderate/trapped) — property losses via the 1936-declaration invalidation mechanism; partial post-2008 restitution
 *   - armenian_apostolic_institutions: target (moderate/trapped) — capital-tax losses, endowment subordination, no seminary
 *   - turkish_jewish_community: target (moderate/constrained) — demographic shrinkage, supervised communal foundation
 *   - orthodox_clergy_candidates: target (powerless/constrained) — formation abroad, citizenship barrier to service
 *   - istanbul_minority_residents: dual-positioned (powerless/constrained) — protected individual worship band, eroding institutional environment
 *   - muslim_majority_institutions: secondary beneficiary (institutional/mobile) — funded, statutorily recognized, advantaged under the same regime
 *   - hellenic_republic: excluded advocate (institutional/arbitrage) — barred from the interpretive conversation, escalates externally
 *   - european_human_rights_bodies: analytical observer (institutional/analytical) — litigation and conditionality, partial remedies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.74).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.72).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive Reading of the Lausanne Minority Protections (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "legal/political/religious").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'fcc405a5-954b-4e87-9941-c8f013d14f4e').
narrative_ontology:cs_kernel_codification('fcc405a5-954b-4e87-9941-c8f013d14f4e', fixed_text).
narrative_ontology:cs_authority_grounding('fcc405a5-954b-4e87-9941-c8f013d14f4e', extraction).
narrative_ontology:cs_interpretation_layer_present('fcc405a5-954b-4e87-9941-c8f013d14f4e').
narrative_ontology:cs_reading_relation('fcc405a5-954b-4e87-9941-c8f013d14f4e', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('fcc405a5-954b-4e87-9941-c8f013d14f4e', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('fcc405a5-954b-4e87-9941-c8f013d14f4e', foundational, worship_rights_exhaust_lausanne_scope).
narrative_ontology:cs_axiom_status(worship_rights_exhaust_lausanne_scope, holdable).
narrative_ontology:cs_axiom_grounding('fcc405a5-954b-4e87-9941-c8f013d14f4e', worship_rights_exhaust_lausanne_scope, conventional).
narrative_ontology:cs_axiom('fcc405a5-954b-4e87-9941-c8f013d14f4e', secondary, minority_institutional_autonomy_threatens_sovereignty).
narrative_ontology:cs_axiom_status(minority_institutional_autonomy_threatens_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('fcc405a5-954b-4e87-9941-c8f013d14f4e', minority_institutional_autonomy_threatens_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('fcc405a5-954b-4e87-9941-c8f013d14f4e', individual_worship_with_domestic_institutional_reserve).
narrative_ontology:cs_drift_state('fcc405a5-954b-4e87-9941-c8f013d14f4e', contemporary_echr_eu_conditionality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fcc405a5-954b-4e87-9941-c8f013d14f4e', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, muslim_majority_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, greek_orthodox_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_apostolic_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, turkish_jewish_community).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, orthodox_clergy_candidates).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, istanbul_minority_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, istanbul_minority_residents).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, domestic_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, lausanne_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the general legal regime under which all foundations, private schools, and religious institutions operate. Determines which parts of the 1923 treaty are implemented domestically, registers and audits minority foundations through the General Directorate of Foundations, sets citizenship and appointment conditions for religious leaders, and receives custody of assets when minority foundations fail its tests. It can reinterpret, relax, or tighten the rules at will and bears none of the costs it imposes.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% The historic center of Orthodox Christianity, resident in Istanbul since before the republic. Recognized domestically only as head of the local Greek minority rather than as an institution with legal personality; its theological academy on Heybeliada has been closed since 1971 under private-school law; candidates for its offices must hold Turkish citizenship; it cannot hold property in its own name. Relocating would sever the see from its city and history; staying means continued inability to form clergy or secure its assets.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate, payer,
    organized, generational, identity_locked, global).

% Community vakıfs running churches, schools, and charities in Istanbul and on the Princes' Islands. Required in 1936 to declare their assets on state forms; courts later ruled that anything acquired after those declarations reverted to the state, and hundreds of properties were transferred to the Treasury or to state-directed endowments. Partial returns followed a 2008 law and a 2010 European court ruling, but the boards remain subordinate to a directorate they do not control, and their schools face closure as enrollment collapses.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, greek_orthodox_foundations, payer,
    moderate, generational, trapped, national).

% The Constantinople Armenian Patriarchate and its network of churches, hospitals, and schools. Bore a disproportionate share of the 1942 capital tax, lost merchants and professionals to deportation-labor and emigration, and manages its remaining endowments under the same foundation regime that has invalidated later acquisitions. It has no functioning theological seminary and forms clergy informally or abroad.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_apostolic_institutions, payer,
    moderate, generational, trapped, national).

% A small recognized community centered in Istanbul, with synagogues, a chief rabbinate, and schools. Emigration to Israel after 1948 and after each successive crisis shrank its base; communal buildings were consolidated into a foundation under state supervision; its schools operate under the private-school regime with minority-school status that restricts enrollment to registered community members.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_jewish_community, payer,
    moderate, generational, constrained, national).

% Young Orthodox men who would staff the patriarchate and its parishes. With the Heybeliada academy shut, formation requires study abroad, yet service at home requires Turkish citizenship and completion of obligations that complicate foreign ordination paths. Most who leave for formation do not return, leaving parishes served by aging priests.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, orthodox_clergy_candidates, payer,
    powerless, biographical, constrained, global).

% Individual members of the Greek, Armenian, and Jewish communities. They may attend services, observe holidays, and send children to community schools — the band of rights the state concedes — while the institutions around them lose property, schools, and leadership. Many have emigrated; those who remain navigate a shrinking institutional landscape.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, istanbul_minority_residents, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, istanbul_minority_residents, beneficiary).

% Mosques, Qur'an schools, and pious foundations administered through state directorates that fund them, appoint their officials, and grant them statutory legal personality. They compete for buildings, students, and charitable income without the disabilities applied to minority counterparts, and state-directed endowments have received transferred minority properties into their portfolio.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, muslim_majority_institutions, beneficiary,
    institutional, generational, mobile, national).

% Neighbor state and treaty signatory that raises minority grievances bilaterally and in European fora. It stands outside the domestic conversation that fixes the treaty's meaning — Ankara holds interpretation to be an internal matter — so its objections register only as diplomatic friction, reciprocal complaints about its own Muslim minority, and litigation support for minority foundations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, hellenic_republic, excluded,
    institutional, generational, arbitrage, continental).

% The Strasbourg court and Council of Europe monitoring organs that hear property and education cases brought by minority foundations. Their judgments have ordered returns and compensation, and accession conditionality produced the 2008 foundation law, but implementation is partial and slow, and they cannot reach the interpretive premise that reserves institutional life to domestic law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels all foundation governance, religious-personnel policy, and minority schooling through a single national legal-administrative framework: one registry of pious foundations, one curriculum inspectorate, one set of citizenship conditions for religious office, and a single point of administrative control over the civil status of every religious institution in the territory.
% TRANSFER_FUNCTION: Moves institutional assets (real property, endowment income), personnel authority (who may be ordained, appointed, and teach), and legal recognition from minority religious communities to the state apparatus and, secondarily, to majority-community institutions operating under state supervision.
% ABSENT_VOICES: The minority communities appear inside the conversation only as petitioners before courts that apply the restrictive premise as settled. Structurally absent are the pre-1923 institutional leadership whose continuity the expansive reading would restore, the guarantor powers whose supervisory role the treaty text names but whose interpretive authority Ankara rejects, and the departed and deported community members whose properties were liquidated without them.
% DISAPPEARANCE_RATIONALE: If the restrictive arrangement vanished overnight — if institutional autonomy, foundation property, and theological education were suddenly guaranteed — minority institutions would reorganize within years: the Heybeliada academy would reopen, property claims would resurface across the foundation registry, legal personality would be asserted, and clergy pipelines would rebuild. The state's control architecture over religious institutional life would have to be dismantled and rebuilt on a different premise.
% FOUNDING_PROBLEM: Securing the new republic's sovereignty over all religious institutional life within its territory: after the Ottoman millet system and the population exchange, minority institutions — above all a patriarchate with universal pretensions and ties to a neighboring state — were classed as potential autonomous power centers aligned with external patrons, and the restrictive reading reserved their property, schools, and leadership formation to general national law.
% FOUNDING_PROBLEM_CORROBORATION: The state attests the problem is live, citing national unity and the security framing in court rulings and interior-ministry statements. Corroboration from outside the benefiting parties runs the other way: European Court of Human Rights judgments in foundation-property and education cases, European Commission progress reports, and US State Department religious-freedom reports all document that the 1920s threat context has receded while the arrangement persists as institutional foreclosure — external judicial and monitoring bodies attest the protective justification no longer fits the facts, and no source outside the beneficiary set attests that it does.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the transfers are decoupled from any service rendered: property moved to the Treasury and state endowments through a declaratory trap, clergy formation was terminated outright, and legal personality is withheld while equivalent majority institutions receive it by statute. Suppression (0.72) is structural — judicial invalidation, administrative refusal, citizenship gating — not interpersonal coercion; it is the raw unscaled property, and the engine scales only extractiveness by directionality and scope. Theater (0.40) reflects a real civil-administrative function coexisting with performative equality rhetoric ('one law for all') and restitutions staged as resolution while the core premise stands. Accessibility collapse (0.62): within-system alternatives are effectively closed — no pathway to personality or property security exists short of leaving the religious category — while external exit (emigration) exists but destroys the institution, so collapse is high but not mountain-grade. Resistance (0.55): five decades of litigation, EU conditionality, and guarantor diplomacy yield partial concessions without ever touching the interpretive premise. The measurement series run on ONE SHARED GRID (t=0..100 step 10) with all three metrics authored at every point. The arc is rise-peak-partial-relaxation-rehardening, not cyclical: enforcement built through the capital-tax and pogrom-expulsion era, peaked at the academy closure and 1974 rulings, partially demobilized under EU conditionality, and re-tightened recently. suppression_requirement is authored because enforcement-capacity change IS the traced dynamic here. The coercion grid is included because level-differentiated coercion is this reading's very content: individual-level suppression stays low (0.20 to 0.25) while organizational-level suppression climbs (0.55 to 0.72) — the protected individual band and the coerced institutional layer are the same arrangement viewed at two levels. Individual- and class-level grid values are conservative judgments where the documentary record is thinner than at the organizational and structural levels (see omega coercion_grid_level_resolution).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute sharply different types from identical structural data. From the state's position the arrangement is neutral domestic legality it built and defends — general law applied uniformly, with the treaty honored 'within its proper scope.' From the trapped and identity-locked minority seats the same structure operates as enforced dispossession: the declaratory trap, the closed academy, the personality denial. Among same-status minority seats, exit options differentiate experience: the patriarchate is identity-locked (relocation equals dissolution), the foundations are trapped (immovable assets, collapsing constituencies), the Jewish community is constrained-but-mobile-leaning (emigration viable, institutions not portable). Istanbul minority residents are genuinely dual-positioned — near-beneficiaries of the narrow protected band, payers through the erosion of everything around it — so their computed seat should sit near symmetric while their institutions sit near full target.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the state apparatus collects forfeited assets, personnel control, and the interpretive monopoly, and its arbitrage-grade exit (it writes the rules) anchors it near the full-beneficiary end; muslim_majority_institutions gain relatively under the same regime at low-to-moderate d. Victim declarations drive high directionality: the patriarchate's identity lock and the foundations' trapped assets push them toward the full-target end, amplified by national scope making verification of enforcement harder; clergy candidates and residents bear high d, with residents moderated toward mid-range by the protected individual band. The excluded and observer seats (hellenic_republic, european_human_rights_bodies) neither collect nor pay and carry analytical or arbitrage positions outside the domestic extraction loop. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct spread without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement presents as ordinary domestic legality — 'one law for all' — which is precisely the cover story that invites rope misclassification. The mandatrophy lens blocks it three ways. First, the founding problem (containment of externally aligned minority power centers in the 1920s) has substantially receded, yet the arrangement persists: if it were a piton, no concentrated capturer would profit and maintenance would be inertial theater; here the state apparatus demonstrably accrues the gains (gain_flow names that seat) and actively defends the premise, which is capture, not neglect. Second, it is not a scaffold: there is no sunset and no transition logic — the 2008 reforms were framed as completion, not as a bridge to self-government. Third, the partial restitutions must not be read as the arrangement winding down: the receipt surface (named capturer, prohibitive fixing cost) shows a captured constraint that concedes assets tactically while holding the interpretive premise, which keeps it snare-flavored under either cost class. The R5 mismatch check (founding_problem_status contested x disappearance_verdict world_rearranges) flags the arrangement as one whose world depends on it while its own justification is disputed — the signature of a live capture, not a resolved mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the lausanne_minority_protections kernel — the restrictive_reading. What would change structurally if a sibling reading were adopted by the interpreting authority?',
    'Adoption of a different reading by the operative interpreter (Turkish courts and administration, or a negotiated international protocol): the expansive sibling moves minority institutions out of the victim set and converts the arrangement toward a coordination profile; the guarantor sibling relocates enforcement to international supervision without changing substantive scope.',
    'The same treaty text supports a snare profile under this reading and materially different profiles under the siblings; cross-reading comparison is valid only at the kernel level, never by merging the files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Kernel-membership committer structure: this story is the restrictive reading; siblings are expansive_reading and guarantor_reading.').

omega_variable(
    treaty_scope_disagreement_location,
    'Where exactly is the reading-contest located in the treaty text — do Articles 40-42 (''establish, manage and control... charitable, religious and social institutions''; schools; language) confer corporate rights on minority institutions, or only permit individual-level activity?',
    'Authoritative interpretation: travaux préparatoires analysis, a treaty-body reading, or a negotiated protocol fixing the scope of Articles 40-42.',
    'If the corporate-rights reading prevails, the victim set empties and the arrangement''s extraction collapses toward coordination cost; if the individual-only reading prevails, this story''s structure stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_scope_disagreement_location, conceptual, 'The specific textual locus (Articles 40-42) on which the restrictive and expansive readings diverge.').

omega_variable(
    reciprocity_validity,
    'Does the reciprocity principle — conditioning minority treatment on Greece''s treatment of its own Muslim minority — have any legal basis under the Lausanne treaty, or is it a unilateral domestic invention invoked as cover?',
    'Comparative treaty-law analysis and drafting history: Lausanne''s minority clauses were not drafted as conditional on reciprocal treatment, and the treaty contains no reciprocity reservation; test whether any authoritative source sustains the conditionality claim.',
    'If reciprocity is legally baseless, the restrictive reading loses its principal external justification and stands exposed as unilateral domestic preference, strengthening the extraction reading of its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_validity, empirical, 'Validity of the reciprocity justification invoked for the restrictive scope.').

omega_variable(
    depletion_vs_enforcement,
    'Is the suppression measured at the interval end sustained by active enforcement machinery, or by prior depletion — a century of emigration that shrank the Istanbul Greek community from over 100,000 to under 2,000 — such that the machinery now guards an emptied field?',
    'Counterfactual relaxation analysis: if enforcement ceased entirely, would institutions regenerate (clergy pipeline, school enrollment, property claims)? Demographic reconstruction of the affected communities, 1923-2023.',
    'If depletion dominates, the current suppression scalar overstates ongoing coercion and understates completed extraction; the arrangement''s present phase is closer to harvesting residue than active repression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depletion_vs_enforcement, empirical, 'Whether end-state suppression reflects live enforcement or depleted targets.').

omega_variable(
    sovereignty_frame_durability,
    'Will the domestic-jurisdiction frame survive sustained European conditionality and litigation, or is the recent re-tightening the frame''s last consolidation?',
    'Political-track observation: EU accession status, Council of Europe implementation proceedings, and whether any future government trades the interpretive premise for external alignment.',
    'If the frame breaks, the arrangement converts toward the guarantor sibling''s enforcement structure and this story''s classification becomes historical; if it holds, the snare profile persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_frame_durability, preference, 'Durability of the domestic-jurisdiction frame under external pressure — a function of political will, not evidence alone.').

omega_variable(
    coercion_grid_level_resolution,
    'How reliably do the individual- and class-level coercion-grid values track lived experience, given that the documentary record concentrates at the organizational and structural levels?',
    'Oral-history and community-survey data on everyday religious life among Istanbul minorities across the interval, particularly 1942-1964, to test whether individual-level pressure spiked above the conservative authored values.',
    'If individual-level pressure was materially higher than authored, the level gradient flattens and the arrangement looks less like a protected-individual-band bargain and more like uniformly distributed coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_grid_level_resolution, conceptual, 'Uncertainty in the leveled grid''s individual and class rows relative to its organizational and structural rows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(laus_tr_t0, observed).
narrative_ontology:measurement(laus_tr_t10, lausanne_minority_protections__restrictive_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(laus_tr_t10, observed).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(laus_tr_t20, observed).
narrative_ontology:measurement(laus_tr_t30, lausanne_minority_protections__restrictive_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(laus_tr_t30, observed).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__restrictive_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(laus_tr_t40, observed).
narrative_ontology:measurement(laus_tr_t50, lausanne_minority_protections__restrictive_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(laus_tr_t50, observed).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__restrictive_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement_basis(laus_tr_t60, observed).
narrative_ontology:measurement(laus_tr_t70, lausanne_minority_protections__restrictive_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement_basis(laus_tr_t70, observed).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__restrictive_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(laus_tr_t80, observed).
narrative_ontology:measurement(laus_tr_t90, lausanne_minority_protections__restrictive_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement_basis(laus_tr_t90, observed).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__restrictive_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement_basis(laus_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(laus_be_t0, observed).
narrative_ontology:measurement(laus_be_t10, lausanne_minority_protections__restrictive_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(laus_be_t10, observed).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(laus_be_t20, observed).
narrative_ontology:measurement(laus_be_t30, lausanne_minority_protections__restrictive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(laus_be_t30, observed).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__restrictive_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement_basis(laus_be_t40, observed).
narrative_ontology:measurement(laus_be_t50, lausanne_minority_protections__restrictive_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(laus_be_t50, observed).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__restrictive_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement_basis(laus_be_t60, observed).
narrative_ontology:measurement(laus_be_t70, lausanne_minority_protections__restrictive_reading, base_extractiveness, 70, 0.83).
narrative_ontology:measurement_basis(laus_be_t70, observed).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__restrictive_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement_basis(laus_be_t80, observed).
narrative_ontology:measurement(laus_be_t90, lausanne_minority_protections__restrictive_reading, base_extractiveness, 90, 0.72).
narrative_ontology:measurement_basis(laus_be_t90, observed).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__restrictive_reading, base_extractiveness, 100, 0.74).
narrative_ontology:measurement_basis(laus_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(laus_su_t0, observed).
narrative_ontology:measurement(laus_su_t10, lausanne_minority_protections__restrictive_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(laus_su_t10, observed).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(laus_su_t20, observed).
narrative_ontology:measurement(laus_su_t30, lausanne_minority_protections__restrictive_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(laus_su_t30, observed).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__restrictive_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement_basis(laus_su_t40, observed).
narrative_ontology:measurement(laus_su_t50, lausanne_minority_protections__restrictive_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement_basis(laus_su_t50, observed).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__restrictive_reading, suppression_requirement, 60, 0.86).
narrative_ontology:measurement_basis(laus_su_t60, observed).
narrative_ontology:measurement(laus_su_t70, lausanne_minority_protections__restrictive_reading, suppression_requirement, 70, 0.8).
narrative_ontology:measurement_basis(laus_su_t70, observed).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__restrictive_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement_basis(laus_su_t80, observed).
narrative_ontology:measurement(laus_su_t90, lausanne_minority_protections__restrictive_reading, suppression_requirement, 90, 0.66).
narrative_ontology:measurement_basis(laus_su_t90, observed).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__restrictive_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(laus_su_t100, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=100
narrative_ontology:measurement(laus_grid_01, lausanne_minority_protections__restrictive_reading, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement_basis(laus_grid_01, observed).
narrative_ontology:measurement(laus_grid_02, lausanne_minority_protections__restrictive_reading, accessibility_collapse(class), 100, 0.6).
narrative_ontology:measurement_basis(laus_grid_02, observed).
narrative_ontology:measurement(laus_grid_03, lausanne_minority_protections__restrictive_reading, accessibility_collapse(individual), 0, 0.25).
narrative_ontology:measurement_basis(laus_grid_03, observed).
narrative_ontology:measurement(laus_grid_04, lausanne_minority_protections__restrictive_reading, accessibility_collapse(individual), 100, 0.3).
narrative_ontology:measurement_basis(laus_grid_04, observed).
narrative_ontology:measurement(laus_grid_05, lausanne_minority_protections__restrictive_reading, accessibility_collapse(organizational), 0, 0.6).
narrative_ontology:measurement_basis(laus_grid_05, observed).
narrative_ontology:measurement(laus_grid_06, lausanne_minority_protections__restrictive_reading, accessibility_collapse(organizational), 100, 0.75).
narrative_ontology:measurement_basis(laus_grid_06, observed).
narrative_ontology:measurement(laus_grid_07, lausanne_minority_protections__restrictive_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement_basis(laus_grid_07, observed).
narrative_ontology:measurement(laus_grid_08, lausanne_minority_protections__restrictive_reading, accessibility_collapse(structural), 100, 0.7).
narrative_ontology:measurement_basis(laus_grid_08, observed).
narrative_ontology:measurement(laus_grid_09, lausanne_minority_protections__restrictive_reading, resistance(class), 0, 0.4).
narrative_ontology:measurement_basis(laus_grid_09, observed).
narrative_ontology:measurement(laus_grid_10, lausanne_minority_protections__restrictive_reading, resistance(class), 100, 0.45).
narrative_ontology:measurement_basis(laus_grid_10, observed).
narrative_ontology:measurement(laus_grid_11, lausanne_minority_protections__restrictive_reading, resistance(individual), 0, 0.2).
narrative_ontology:measurement_basis(laus_grid_11, observed).
narrative_ontology:measurement(laus_grid_12, lausanne_minority_protections__restrictive_reading, resistance(individual), 100, 0.25).
narrative_ontology:measurement_basis(laus_grid_12, observed).
narrative_ontology:measurement(laus_grid_13, lausanne_minority_protections__restrictive_reading, resistance(organizational), 0, 0.5).
narrative_ontology:measurement_basis(laus_grid_13, observed).
narrative_ontology:measurement(laus_grid_14, lausanne_minority_protections__restrictive_reading, resistance(organizational), 100, 0.55).
narrative_ontology:measurement_basis(laus_grid_14, observed).
narrative_ontology:measurement(laus_grid_15, lausanne_minority_protections__restrictive_reading, resistance(structural), 0, 0.5).
narrative_ontology:measurement_basis(laus_grid_15, observed).
narrative_ontology:measurement(laus_grid_16, lausanne_minority_protections__restrictive_reading, resistance(structural), 100, 0.65).
narrative_ontology:measurement_basis(laus_grid_16, observed).
narrative_ontology:measurement(laus_grid_17, lausanne_minority_protections__restrictive_reading, stakes_inflation(class), 0, 0.5).
narrative_ontology:measurement_basis(laus_grid_17, observed).
narrative_ontology:measurement(laus_grid_18, lausanne_minority_protections__restrictive_reading, stakes_inflation(class), 100, 0.65).
narrative_ontology:measurement_basis(laus_grid_18, observed).
narrative_ontology:measurement(laus_grid_19, lausanne_minority_protections__restrictive_reading, stakes_inflation(individual), 0, 0.3).
narrative_ontology:measurement_basis(laus_grid_19, observed).
narrative_ontology:measurement(laus_grid_20, lausanne_minority_protections__restrictive_reading, stakes_inflation(individual), 100, 0.35).
narrative_ontology:measurement_basis(laus_grid_20, observed).
narrative_ontology:measurement(laus_grid_21, lausanne_minority_protections__restrictive_reading, stakes_inflation(organizational), 0, 0.6).
narrative_ontology:measurement_basis(laus_grid_21, observed).
narrative_ontology:measurement(laus_grid_22, lausanne_minority_protections__restrictive_reading, stakes_inflation(organizational), 100, 0.8).
narrative_ontology:measurement_basis(laus_grid_22, observed).
narrative_ontology:measurement(laus_grid_23, lausanne_minority_protections__restrictive_reading, stakes_inflation(structural), 0, 0.5).
narrative_ontology:measurement_basis(laus_grid_23, observed).
narrative_ontology:measurement(laus_grid_24, lausanne_minority_protections__restrictive_reading, stakes_inflation(structural), 100, 0.6).
narrative_ontology:measurement_basis(laus_grid_24, observed).
narrative_ontology:measurement(laus_grid_25, lausanne_minority_protections__restrictive_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement_basis(laus_grid_25, observed).
narrative_ontology:measurement(laus_grid_26, lausanne_minority_protections__restrictive_reading, suppression(class), 100, 0.6).
narrative_ontology:measurement_basis(laus_grid_26, observed).
narrative_ontology:measurement(laus_grid_27, lausanne_minority_protections__restrictive_reading, suppression(individual), 0, 0.2).
narrative_ontology:measurement_basis(laus_grid_27, observed).
narrative_ontology:measurement(laus_grid_28, lausanne_minority_protections__restrictive_reading, suppression(individual), 100, 0.25).
narrative_ontology:measurement_basis(laus_grid_28, observed).
narrative_ontology:measurement(laus_grid_29, lausanne_minority_protections__restrictive_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement_basis(laus_grid_29, observed).
narrative_ontology:measurement(laus_grid_30, lausanne_minority_protections__restrictive_reading, suppression(organizational), 100, 0.72).
narrative_ontology:measurement_basis(laus_grid_30, observed).
narrative_ontology:measurement(laus_grid_31, lausanne_minority_protections__restrictive_reading, suppression(structural), 0, 0.45).
narrative_ontology:measurement_basis(laus_grid_31, observed).
narrative_ontology:measurement(laus_grid_32, lausanne_minority_protections__restrictive_reading, suppression(structural), 100, 0.6).
narrative_ontology:measurement_basis(laus_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Lausanne minority protections' (epsilon-invariance principle). The label conflates two structurally distinct axes: substantive scope (do the articles guarantee institutional continuity, or only individual worship?) and enforcement locus (domestic interpretation, or internationally supervised obligation?). This file authors the restrictive instantiation on the scope axis with the domestic locus assumed; the expansive sibling shares the referent but authors a near-zero-extraction institutional-continuity constraint; the guarantor sibling brackets scope and authors an enforcement-locus constraint. Upstream/downstream: the expansive reading is cited as the treaty's evident meaning by advocates, while this restrictive reading is the operative official one — each sibling file documents the epsilon divergence from its own seat. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
