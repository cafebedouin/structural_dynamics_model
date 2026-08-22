% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage (Museum/Tourist Site)
 *   domain: cultural/religious/political
 *
 * SUMMARY:
 *   The Hagia Sophia in Istanbul has been the subject of intense contestation
 *   over its institutional form and religious status. Built in 537 CE as a
 *   Byzantine Christian cathedral, it served as a mosque for 567 years after
 *   the 1453 Ottoman conquest, was converted to a museum in 1935 by the
 *   secular Turkish republic, and in 2020 was reconverted to a mosque by
 *   Turkish state action. This constraint story analyzes the UNIVERSAL
 *   HERITAGE READING—the claim that the site's legitimacy derives from its
 *   status as shared human cultural heritage transcending any single
 *   religious or national claim. Under this reading, the site should be
 *   preserved as a museum/UNESCO heritage site, accessible to all traditions
 *   but controlled by none, administered by secular expertise and
 *   international heritage frameworks. This reading benefits global tourism
 *   operators, secular Turkish elites, and transnational heritage
 *   institutions; it suppresses Islamic worship claims (framed as sectarian
 *   appropriation) and Orthodox restitution claims (framed as
 *   backward-looking). The reading's extractiveness (0.68) derives both from
 *   tourism revenue concentration and from ideological suppression of
 *   religious authority. The constraint's theater ratio (0.58) reflects that
 *   preservation and conservation are real functions, but a growing share of
 *   enforcement effort defends the suppression of religious use rather than
 *   the preservation function itself. The reading coexists with two sibling
 *   readings—the Islamic sovereignty reading (Ottoman waqf and state
 *   authority) and the Orthodox restitution reading (Byzantine foundation and
 *   ecclesiastical claim)—in a kernel contest where all three remain live
 *   positions held by different parties.
 *
 * KEY AGENTS:
 *   - Museum administration: enforces the secular heritage framework; acts as the constraint's agenda-setter
 *   - Global tourism/scholarship sector: benefits from universal-heritage status; collects revenue and prestige
 *   - Secular Turkish elites: benefits from ideological signal of Turkish modernity; national identity intertwined with the constraint
 *   - Islamic worship constituencies: victims; their claim is suppressed and redefined as sectarian
 *   - Orthodox restitution advocates: victims; their claim is suppressed and redefined as backward
 *   - Turkish state apparatus: sets the rules and enforcement machinery; agent and beneficiary
 *   - UNESCO/transnational heritage institutions: beneficiary; depends on universal-heritage doctrine to legitimate their role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage (Museum/Tourist Site)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural/religious/political").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'c7352c16-2652-4306-ad09-e33c68d6ab67').
narrative_ontology:cs_kernel_codification('c7352c16-2652-4306-ad09-e33c68d6ab67', fixed_text).
narrative_ontology:cs_authority_grounding('c7352c16-2652-4306-ad09-e33c68d6ab67', extraction).
narrative_ontology:cs_interpretation_layer_present('c7352c16-2652-4306-ad09-e33c68d6ab67').
narrative_ontology:cs_reading_relation('c7352c16-2652-4306-ad09-e33c68d6ab67', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7352c16-2652-4306-ad09-e33c68d6ab67', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('c7352c16-2652-4306-ad09-e33c68d6ab67', foundational, heritage_transcends_sectarian_claims).
narrative_ontology:cs_axiom_status(heritage_transcends_sectarian_claims, holdable).
narrative_ontology:cs_axiom_grounding('c7352c16-2652-4306-ad09-e33c68d6ab67', heritage_transcends_sectarian_claims, conventional).
narrative_ontology:cs_axiom('c7352c16-2652-4306-ad09-e33c68d6ab67', foundational, secular_expertise_legitimate_steward).
narrative_ontology:cs_axiom_status(secular_expertise_legitimate_steward, holdable).
narrative_ontology:cs_axiom_grounding('c7352c16-2652-4306-ad09-e33c68d6ab67', secular_expertise_legitimate_steward, deontological).
narrative_ontology:cs_reference_frame('c7352c16-2652-4306-ad09-e33c68d6ab67', universal_heritage_authority_framework).
narrative_ontology:cs_drift_state('c7352c16-2652-4306-ad09-e33c68d6ab67', contemporary_post_2020_sovereignty_reassertion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7352c16-2652-4306-ad09-e33c68d6ab67', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secular_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_framework).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_constituencies).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_restitution_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, transnational_heritage_institutions).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, turkish_state_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages the site as a museum and UNESCO World Heritage Site. Makes decisions about access, interpretation, religious ceremonies, visitor flows, and the constraint's enforcement. Frames the arrangement as preserving universal heritage for humanity while neutrally honoring multiple traditions. Directly controlled by Turkish state secular constitutional framework and international heritage agreements.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, museum_administration, agenda_setter,
    institutional, generational, analytical, global).

% Tourism operators, academic institutions, museums worldwide, and UNESCO derive economic and symbolic value from the site's status as a secular, universally accessible heritage monument. Revenue flows from tourism; prestige accrues from being custodians of a 'transcultural' legacy. Would lose substantial income and institutional standing if the site reverted to exclusive religious use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector, beneficiary,
    organized, generational, arbitrage, global).

% Political and intellectual elites in Turkey who constructed modern Turkish national identity on secular foundations. The site-as-museum symbolizes Turkish modernity and scientific rationalism. Frames Islamic worship there as backward-looking. Benefits from the constraint's ideological signal that Turkey has transcended religious sectarianism in favor of universal civilization.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secular_turkish_elites, beneficiary,
    powerful, generational, mobile, national).

% Turkish Muslim believers for whom the site is historically a mosque and spiritually a center of Islamic devotion under 567 years of Ottoman endowment (waqf). The constraint suppresses their legally-claimed access to worship there. They must advocate for reversion to Islamic use, but the museum frame and international heritage designation make exit from the constraint (returning the site to worship) nearly impossible without coordinated political action. Their religious claim and memory are categorized as 'parochial' under the universal-heritage framing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_constituencies, payer,
    powerful, generational, constrained, national).

% Orthodox ecclesiastical authorities and Byzantinists who argue the site should revert to Christian control or remain permanently neutral to honor its 916-year Byzantine foundation. Their claim is suppressed by the same universal-heritage framing that suppresses Islamic claims—both are read as 'sectarian' threats to secular custody. They lack power to enforce exit from the constraint.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_restitution_advocates, payer,
    moderate, generational, constrained, global).

% UNESCO, international conservation bodies, and cosmopolitan academic networks that depend on the universal-heritage doctrine to legitimate their authority over sites of contested significance. The constraint's enforcement protects their role as neutral arbiters and their ability to claim cultural properties as belonging to 'humanity' rather than to specific religious or national communities.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, transnational_heritage_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% The Turkish state (distinct from secular elites, though overlapping) has constitutional and legal authority over the site. Benefits from the constraint's framing as a matter of national heritage rather than religious sovereignty, avoiding both Islamic-governance claims and international pressure to restore Christian worship. Museum administration is its enforcement arm.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, turkish_state_apparatus, beneficiary).

% The international heritage and UNESCO framework that treats cultural sites as transcending national and religious claims, enabling technocratic management under universal-civilization doctrine. A non-agent entity kept for narrative completeness; the framework itself collects no rents but enables and legitimizes the constraint's operation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_law_and_treaty_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__universal_heritage_reading, international_law_and_treaty_framework).

% Religious communities (Islamic and Orthodox) whose fundamental claim—that the site should serve their respective faith traditions—is structurally excluded from the negotiation space by the universal-heritage framing, which redescribes religious use as incompatible with 'open access' and cultural preservation. They are present in the political landscape but their core claim is pre-emptively ruled out as inadmissible.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, excluded_worship_claimants, excluded,
    powerful, generational, constrained, national).

% Turkish courts and constitutional authorities that interpret the secular constitutional framework as the legitimate authority over the site. They have evaluated and rejected religious claims to the site as conflicts with secular national law and UNESCO obligations. Their position is analytically privileged; their role is to adjudicate the boundary between permissible heritage management and prohibited sectarian use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secular_constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a structurally significant historical monument against degradation, religious vandalism, and exclusive appropriation by making it accessible to scholars, pilgrims of all traditions, and the public under expert conservation management rather than under control by any single religious authority.
% TRANSFER_FUNCTION: Moves the site's symbolic capital and economic value from local religious communities (Islamic and Orthodox) to the global tourism and heritage sectors and to secular state elites. Islamic and Orthodox claims to exclusive use are suppressed in exchange for 'open' but administered access framed as universal benefit. Economic flows generated by tourism revenue accrue to the state and international tourism operators; spiritual authority over the space is centralized in secular curatorial hands.
% ABSENT_VOICES: Islamic mosque authorities and Orthodox ecclesiastical leadership would argue (and do argue in suppressed form) that they hold prior and deeper claims to the site—the mosque constituency based on 567 years of Ottoman waqf and continuous worship; the Orthodox on 916 years of founding as a cathedral. Both are excluded from the primary governance structure and their claims are pre-emptively redefined as 'sectarian' and incompatible with the constraint.
% DISAPPEARANCE_RATIONALE: If the constraint and its universal-heritage justification vanished, the site would almost certainly revert to Islamic worship use under Turkish state authority (Islam is the majority faith in Turkey and the Ottoman-era waqf legal claim is stronger in Turkish constitutional law than it is under UNESCO doctrine). Alternatively, the Turkish state might accept Orthodox restitution claims or negotiate a shared worship arrangement. The constraint's disappearance would reallocate the site from global tourism/scholarship to localized religious authority, collapsing tourism revenue and symbolic value.
% FOUNDING_PROBLEM: After the 1953 conversion from mosque to museum, the question became: who controls the site and under what legitimacy framework? Religious claimants (Islamic and Orthodox) each asserted historical right; Turkish nationalism asserted state sovereignty; the international community asserted heritage preservation duty. The universal-heritage reading solves the founding problem by subordinating all three claims to a technocratic, secular, universalist authority that claims to serve all traditions equally.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish secular state and UNESCO assert the founding problem is solved—the site is preserved, accessible, and neutral. Islamic constituencies dispute this, asserting the founding problem of Islamic worship restoration remains live. Orthodox advocates dispute it as well, arguing the site should honor its Byzantine heritage either through restitution or permanent neutrality. Independent scholarship on Ottoman waqf law and Byzantine historical claims (from outside the benefiting secular-heritage sector) corroborates that the founding problem is not resolved but rather that the universal-heritage reading has suppressed it by redefining religious claims as inadmissible rather than as solved.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as tangled_rope (real coordination function: preservation and multi-tradition access coexist with extraction), but the authored metrics describe substantial extractiveness and high suppression—these are independently authored facts. Extractiveness of 0.68 reflects that the constraint concentrates tourism revenue and symbolic capital in the hands of secular state and transnational tourism operators while suppressing Islamic and Orthodox claims to use the space according to their traditions. Tourism revenue is decoupled from any obligation to support the religious constituencies whose historical claims are suppressed. Suppression of 0.72 reflects that the constraint's persistence depends on active enforcement via Turkish law, UNESCO designation, and international heritage treaties—religious constituencies cannot access worship space without overriding multiple layers of legal and bureaucratic barriers. Theater ratio of 0.58 indicates that a substantial (but minority) fraction of the constraint's maintenance effort is devoted to performative justification—the universal-heritage rhetoric, the cosmopolitan framing, the neutrality claims—rather than to the actual preservation function. As time progresses (measurements from 0 to 71), extractiveness and suppression both rise initially and stabilize, suggesting the constraint hardened through the interval (enforcement machinery became more robust, international heritage agreements more entrenched) but did not degrade. The measurements are authored on a single shared time grid: every metric has a value at every examined time point (0, 8, 16, 24, 32, 40, 50, 60, 71), ensuring alignment. The claim/metric divergence is intentional and structurally meaningful: the engine will compute per-seat types from the structural data (stakeholder roles + directionality), and the divergence between the claimed type and the computed type is exactly what the apparatus measures.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (museum administration / Turkish state) and the beneficiary seats (tourism sector, secular elites) perceive the constraint as genuine coordination—they have solved the problem of preserving a contested monument and enabling access to it by multiple constituencies, albeit under expert secular management. From their seats, the constraint enables tourism revenue, scholarly access, and symbolism of Turkish modernity without submitting to any single religious authority. The payer seats (Islamic worship constituencies, Orthodox advocates) perceive the same constraint as enforced suppression—their fundamental claim (that the site should serve their respective religious communities) is not accommodated or compromised; it is pre-emptively ruled out as incompatible with the universal-heritage framework itself. The engine computes these divergences from the structural data: the state/tourism sector sits at lower directionality (beneficiaries, d near 0.0–0.3), while religious constituencies sit at higher directionality (suppressed targets, d near 0.7–1.0). The divergence is not a data error; it is the signature of the constraint's asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Museum administration (institutional power, mobile exit, analytical horizon) sits at the agenda-setter role and low directionality—it sets the rules and benefits from the constraint's persistence. Global tourism/scholarship sector (organized power, arbitrage exit) sits at beneficiary and similarly low directionality—they collect revenue and prestige. Secular Turkish elites (powerful institutional power, mobile exit, generational horizon) sit at beneficiary and face a subtle directionality question: they are raw beneficiaries of the ideological signal the constraint provides (confirmation of Turkish modernity), but their exit options are mobile—they could in principle accept Islamic or Orthodox restitution and retain power. However, the omega variable on elite identity-lock probes whether their professional and national identity is bound to the constraint such that exit is unavailable even if nominally mobile. Islamic worship constituencies (powerful at scale, constrained exit due to religious obligation and cultural identity, generational/civilizational horizon) sit at payer and face high directionality—they are suppressed targets with few exit options. Orthodox restitution advocates (moderate power, constrained exit, generational horizon) also sit at payer with high directionality—they are suppressed and trapped. The Turkish state apparatus is both agenda-setter (it enforces) and beneficiary (it avoids the sectarian governance problem), with institutional power and mobile exit, so directionality is low. Directionality overrides are not needed here: the structural data (beneficiary/victim roles + exit options + power atoms) derive the correct d-values without manual correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows classic mandatrophy: the founding problem (preservation of a contested monument) and the founding problem status (contested—religious constituencies dispute that the problem is solved) together signal that the constraint's mandate has outlived its narrow justification. The constraint was built to solve the 1935 problem of converting the mosque to a museum to avoid sectarian governance—a legitimate coordination problem in the context of early republican Turkey. By 2020, the founding problem is widely seen as not-solved by the parties who bear the constraint: Islamic constituencies dispute that the constraint preserves the site adequately, and Orthodox constituencies dispute that it honors the site's heritage. The constraint's persistence despite contested mandate reflects institutional inertia and the interests of beneficiary parties (tourism, secular elites, UNESCO) rather than continued agreement that the constraint solves a live problem. The theater ratio (0.58) confirms mandatrophy: the constraint maintains a substantial performance apparatus (the universal-heritage rhetoric, the neutrality claims, the cosmopolitan framing) to justify its persistence in the face of suppressed religious claims. A high theater ratio combined with contested founding-problem status is a diagnostic marker of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_universal_vs_religious_sovereignty,
    'Is the universal-heritage reading''s core premise (the site belongs to humanity and transcends sectarian claims) logically incompatible with the religious-sovereignty readings (Islamic waqf claim, Orthodox restitution claim), or do all three represent competing but potentially coexistent legitimate frameworks?',
    'Genealogical and philosophical analysis of the kernel: Does the universal-heritage reading assert that sectarian use is metaphysically impossible/unthinkable, or that it is morally/legally prohibited by international law? The first would forecast foreclosure; the second would forecast coexistence. Examine the reading''s actual deployment in UNESCO policy and Turkish court rulings.',
    'If the reading forecloses (asserts incompatibility in principle), the sibling readings are architecturally excluded rather than suppressed—they remain live claims but the reading structure rules them out. If the reading merely influences or coexists, then suppression of Islamic/Orthodox worship is a separate enforcement mechanism, not a structural entailment of the reading itself. This distinction determines whether the constraint''s classification changes if enforcement weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_universal_vs_religious_sovereignty, conceptual, 'Whether the universal-heritage reading logically forecloses religious-sovereignty readings or merely suppresses them via enforcement.').

omega_variable(
    extraction_mechanism_tourism_vs_ideological,
    'Is the measured extractiveness (0.68) driven primarily by tourism revenue concentration (economic extraction that could theoretically be distributed via taxation to fund worship spaces) or by ideological suppression of religious claims (non-economic transfer of authority and legitimacy)?',
    'Comparative case analysis: sites where tourism revenue is taxed/redistributed to religious communities (e.g., Western Church-state accommodation models) versus sites where religious claims are suppressed regardless of revenue sharing (Chinese temples under atheist state, Hindu sites under secularist museum frameworks). If extraction persists even when revenue-sharing increases, the mechanism is ideological, not economic.',
    'If extraction is primarily economic (revenue capture), the constraint might be amenable to redistribution without fundamentally compromising the universal-heritage reading. If extraction is primarily ideological (subordination of religious authority to secular expertise), then no revenue-sharing arrangement resolves the victim set''s core suppression—the constraint is architecturally hostile to Islamic/Orthodox governance claims, not merely to unfair profit allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_tourism_vs_ideological, empirical, 'Whether the constraint''s extractiveness is economic (revenue) or ideological (authority suppression).').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) maintained by structural barriers (Turkish law, UNESCO enforcement, international heritage treaties) or by internalized delegitimation (religious constituencies have been persuaded that their claims are ''sectarian'' and incompatible with modern rationality)?',
    'Counterfactual relaxation experiment: if UNESCO treaties were revised to permit Ottoman waqf claims or if international law recognized restitution rights, would Islamic/Orthodox constituencies immediately mobilize, or has decades of suppression under the universal-heritage reading created self-reinforcing delegitimation? Post-exit trajectory analysis: if constraints loosened, how quickly would suppression reverse?',
    'If suppression is structural, removing or relaxing the enforcement machinery would enable rapid reversion to religious use. If suppression is internalized, the constraint''s persistence becomes self-perpetuating even if enforcement weakens—religious constituencies would need to reconstruct their own legitimacy narratives to claim the site. Internalized suppression is higher-impact and more difficult to reverse via policy change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of religious claims is maintained by external enforcement or internalized delegitimation.').

omega_variable(
    secular_elite_identity_lock,
    'For secular Turkish elites whose national and professional identity is bound to the universal-heritage reading, would loss of the constraint (reversion to Islamic worship or Orthodox restitution) constitute loss of face, loss of status, or loss of core identity?',
    'Elite discourse analysis: examine how central the site-as-museum is to Turkish modernist self-conception. Interview elites about counterfactual scenarios (what if the site reverted to a mosque, what would that mean for Turkey''s place in the world?). Assess whether exit from the constraint is available to elites or whether their identity is locked to its persistence.',
    'If the constraint is identity-locked for beneficiary elites, their directionality d-value should be lower (beneficiary, but trapped by self-conception) than raw economic benefit alone would suggest. Identity-lock extends the constraint''s stability beyond what economic incentives alone would predict and makes negotiated resolution more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_elite_identity_lock, empirical, 'Whether secular elites'' identity is locked to the constraint''s persistence.').

omega_variable(
    universal_heritage_as_colonial_episteme,
    'Is the universal-heritage reading a genuinely neutral framework, or is it a repackaging of secular Western epistemic authority that treats religious knowledge and claims as ''sectarian'' by definition?',
    'Genealogy of the universal-heritage doctrine: trace its origins to 18th–20th-century European rationalism and museum practice. Compare how the doctrine treats Christian vs. Muslim vs. Hindu sites: does it universally suppress religious claims, or does it accord secular/rationalist claims (archaeological, historical, artistic) superior epistemic status? Examine whether the reading privileges a particular (secular-Western) framework while presenting itself as framework-neutral.',
    'If the universal-heritage reading is itself a cultural episteme rather than a neutral truth, the constraint''s claim to transcend sectarianism is false—it has simply elevated one sectarian framework (secular rationalism) to the status of universal truth. This does not change the constraint''s classification but reframes its beneficiary set: secular Western elites + secular-aligned non-Western elites (not ''humanity'') benefit from the constraint, while communities whose episteme treats sacred sites as requiring religious rather than curatorial authority are suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_heritage_as_colonial_episteme, conceptual, 'Whether the universal-heritage reading is epistemically neutral or a covert assertion of secular-Western epistemic authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 71).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hagi_tr_t8, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 8, 0.47).
narrative_ontology:measurement(hagi_tr_t16, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement(hagi_tr_t32, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 32, 0.56).
narrative_ontology:measurement(hagi_tr_t40, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement(hagi_tr_t50, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 60, 0.59).
narrative_ontology:measurement(hagi_tr_t71, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 71, 0.58).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hagi_be_t8, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(hagi_be_t16, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(hagi_be_t32, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(hagi_be_t40, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(hagi_be_t50, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(hagi_be_t71, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 71, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hagi_su_t8, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(hagi_su_t16, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(hagi_su_t24, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(hagi_su_t32, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(hagi_su_t40, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(hagi_su_t50, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(hagi_su_t71, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 71, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.18).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia substrate decomposes into three structurally distinct constraints, each instantiating a different reading of the contested kernel. ε values differ: the universal-heritage reading (this file) extracts via tourism/ideological suppression; the Islamic-sovereignty reading extracts via waqf-claim suppression of Orthodox/secular claims; the Orthodox-restitution reading extracts via suppression of Ottoman sovereignty and Islamic worship claims. Each reading has different beneficiaries, different victims, and different authority groundings. The three constraints are linked via network.affects_constraints to model the shared kernel and the mutual exclusivity/coexistence relationships mediated by cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
