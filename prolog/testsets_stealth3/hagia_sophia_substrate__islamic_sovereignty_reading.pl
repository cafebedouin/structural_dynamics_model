% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Reading — Conquest-Waqf Custody Arrangement
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   Since July 2020 the building operates as a congregational mosque
 *   administered by the state's religious directorate under a presidential
 *   decree issued hours after the Council of State annulled the 1934 museum
 *   conversion on the ground that it exceeded the founder's endowment
 *   conditions. The arrangement's legitimacy narrative, as this reading holds
 *   it: title derives from the 1453 conquest and the continuous pious
 *   endowment established by the conqueror; the intervening museum period was
 *   a lawful-order defect now corrected; the site is sovereign Islamic
 *   worship space under Turkish state authority. Operationally the
 *   arrangement maintains worship priority (five daily prayers, seasonal
 *   observances), mediates visitor access through gallery routing,
 *   prayer-hour closures, and mosaic concealment during services, charges
 *   foreign visitors a fee reinstated in 2024, and declines the international
 *   heritage regime's pre-decision consultation. The claim/metric gap is
 *   deliberate: this reading CLAIMS the arrangement as rightful restoration
 *   of an endowment obligation, while the authored metrics describe its
 *   actual operation as moderately-high extraction with active enforcement —
 *   the engine computes the per-seat verdicts from the structural data. KEY
 *   AGENTS (by structural relationship): - akp_political_coalition: primary
 *   beneficiary (institutional/arbitrage) — collects political consolidation
 *   rents - turkish_presidency: agenda setter (institutional/arbitrage) —
 *   authored the decree, controls custody terms -
 *   council_of_state_ninth_chamber: agenda setter, enforcement arm
 *   (institutional/constrained) — judicial validation -
 *   diyanet_administration: agenda setter + beneficiary
 *   (institutional/arbitrage) — daily operations, custodial gains -
 *   turkish_islamic_constituency: beneficiary (organized/mobile) — regained
 *   congregational worship - global_sunni_ummah: symbolic beneficiary
 *   (moderate/mobile) - non_muslim_visitors: payer (moderate/mobile) — bears
 *   access restrictions - secularist_turks: payer (organized/identity_locked)
 *   — bears ideological defeat of the laic settlement -
 *   ecumenical_patriarchate: payer (moderate/trapped) — restitution path
 *   closed - unesco_world_heritage_regime: payer (institutional/constrained)
 *   — jurisdiction denied - hellenic_foreign_ministry: excluded
 *   (institutional/mobile) — objects outside the deciding forum
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.58).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Reading — Conquest-Waqf Custody Arrangement").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'f6c364f2-8e73-4af6-ac32-317359fb1624').
narrative_ontology:cs_kernel_codification('f6c364f2-8e73-4af6-ac32-317359fb1624', fixed_text).
narrative_ontology:cs_authority_grounding('f6c364f2-8e73-4af6-ac32-317359fb1624', lineage).
narrative_ontology:cs_interpretation_layer_present('f6c364f2-8e73-4af6-ac32-317359fb1624').
narrative_ontology:cs_reading_relation('f6c364f2-8e73-4af6-ac32-317359fb1624', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('f6c364f2-8e73-4af6-ac32-317359fb1624', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('f6c364f2-8e73-4af6-ac32-317359fb1624', foundational, conquest_endowment_confers_perpetual_title).
narrative_ontology:cs_axiom_status(conquest_endowment_confers_perpetual_title, holdable).
narrative_ontology:cs_axiom_grounding('f6c364f2-8e73-4af6-ac32-317359fb1624', conquest_endowment_confers_perpetual_title, conventional).
narrative_ontology:cs_axiom('f6c364f2-8e73-4af6-ac32-317359fb1624', foundational, prior_ecclesial_dedication_superseded_by_conquest).
narrative_ontology:cs_axiom_status(prior_ecclesial_dedication_superseded_by_conquest, holdable).
narrative_ontology:cs_axiom_grounding('f6c364f2-8e73-4af6-ac32-317359fb1624', prior_ecclesial_dedication_superseded_by_conquest, conventional).
narrative_ontology:cs_axiom('f6c364f2-8e73-4af6-ac32-317359fb1624', secondary, state_custody_discharges_founder_intent).
narrative_ontology:cs_axiom_status(state_custody_discharges_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('f6c364f2-8e73-4af6-ac32-317359fb1624', state_custody_discharges_founder_intent, instrumental).
narrative_ontology:cs_reference_frame('f6c364f2-8e73-4af6-ac32-317359fb1624', conquest_endowment_continuity).
narrative_ontology:cs_drift_state('f6c364f2-8e73-4af6-ac32-317359fb1624', post_2020_reconversion, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('f6c364f2-8e73-4af6-ac32-317359fb1624', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, global_sunni_ummah).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governing coalition that pursued mosque restoration from its 1990s Istanbul mayorship onward; the 2020 reconversion delivered a signature promise to its religious-conservative base shortly after it lost the Istanbul mayorality. Collects consolidation capital at each anniversary and election cycle. Its exposure is reputational rather than material: it authored the arrangement and can redeploy the symbolic asset or renegotiate its terms at will.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary,
    institutional, generational, arbitrage, national).

% Issued the July 2020 presidential decree transferring site administration to the Directorate of Religious Affairs immediately after the court reversal, and controls access policy, scheduling, and the official conquest-waqf legitimacy narrative. Enforces the arrangement through allied institutions and sets the terms on which every other seat operates.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_presidency, agenda_setter,
    institutional, generational, arbitrage, national).

% Judicial arm that annulled the 1934 cabinet decree converting the site to a museum, reasoning that the transfer exceeded the founder's endowment conditions. Its ruling supplied the legal form in which the reconversion presented itself as restoration of lawful endowment status rather than a new political act, and it continues to decide individual applications flowing from the restored status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, council_of_state_ninth_chamber, agenda_setter,
    institutional, biographical, constrained, national).

% Directorate of Religious Affairs that took over daily custodial operation: appointing clergy, scheduling five daily prayers, running Ramadan programs, and administering the visitor-routing protocols — gallery separation, prayer-hour closures, mosaic concealment during services — through which non-Muslim access is mediated. Gains flagship custody, expanded staffing salience, and the fee stream introduced for foreign visitors in 2024.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, beneficiary).

% Sunni worshippers who regained unrestricted congregational access to a building closed to communal prayer since 1934. Attend daily prayers and seasonal observances in large numbers; their participation supplies the demand side of the consolidation exchange. Other mosques remain available to them, so their benefit is additive rather than captive.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% Diffuse transnational community that receives the reconversion symbolically: it reads worldwide as restoration of an iconic conquest-era endowment. No material flows to it depend on the arrangement; the benefit is identitarian and vicarious, and its attention can shift freely, which caps any extraction that could be aimed at it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, global_sunni_ummah, beneficiary,
    moderate, civilizational, mobile, global).

% Tourists and non-Muslim pilgrims who encounter prayer-hour closures, gallery-only routing, mosaic concealment during services, and a visitor fee reintroduced for foreigners in 2024. They bear the access and experiential costs of worship priority. Their individual leverage is weak because substitution is cheap: other monuments and destinations absorb displaced visits.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    moderate, biographical, mobile, global).

% Republican-establishment citizens, including a large Alevi constituency, for whom the reversal undid the 1934 laic settlement they understand as constitutive of the republic. Opposition parties, bar associations, and professional groups contested the decree in court and in the street and lost. They cannot exit citizenship or the national symbolic order, so they live inside an arrangement they can neither accept nor leave; their identity is bound to the settlement the arrangement displaced.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, generational, identity_locked, national).

% Historic see of the Orthodox communion, headquartered near the site, whose restitution aspiration and minority-rights advocacy were overridden by the court reversal. It cannot relocate its canonical seat and depends on Turkish governmental goodwill for its own precarious legal standing, which disciplines the tone of its public grief. Its dispersed flock receives the closure of the restitution path as a denominational injury.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate, payer,
    moderate, civilizational, trapped, global).

% Custodian of the World Heritage inscription covering the Historic Areas of Istanbul. It received no advance notification of the status change and possesses no enforcement lever beyond reactive monitoring, retention-on-list deliberation, and reputational instruments. The jurisdictional claim the arrangement denies is precisely the regime's operative asset; withdrawing the inscription would penalize the site rather than the state.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime, payer,
    institutional, civilizational, constrained, global).

% Greek state organ that objects diplomatically around each anniversary and pursues protests through bilateral and European channels, but holds no seat in the Turkish domestic process where custody was decided. Its exclusion from the deciding forum is structural: the arrangement's sovereignty premise defines the decision as an internal matter.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, hellenic_foreign_ministry, excluded,
    institutional, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns unified custodianship of a singular, heavily contested sacred structure: one authority schedules congregational worship, conserves the fabric, and routes millions of visitors, instead of competing claimants litigating access ad hoc. Within the reading, it also discharges a standing endowment obligation to keep the founder's worship purpose continuously served.
% TRANSFER_FUNCTION: Moves political consolidation capital from the site's universal and historic standing to the governing coalition; moves physical access priority from the general public to the defined worship community; moves a visitor fee stream from foreign tourists to the state religious administration; and moves jurisdictional deference away from the international heritage regime to the Turkish state.
% ABSENT_VOICES: Non-Muslim visitor publics were not consulted; the Ecumenical Patriarchate learned of the decision through the press; the World Heritage regime received no advance notification; Alevi organizations, secularist bar associations, and opposition parties objected only after the fact through courts and demonstrations without a seat in the deciding process; the Greek state is confined to diplomatic protest outside the forum. The unanimity of the decision record reflects the absence of these seats from the room, not their absence of objection.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — the site reverting to neutral museum status — the governing coalition would lose a signature consolidation asset and a fulfilled base pledge, the religious directorate would lose flagship custody and its fee stream, worshippers would lose the regained prayer space, the heritage regime would regain consultation standing, and non-Muslim access would normalize: multiple seats' arrangements demonstrably depend on the current configuration.
% FOUNDING_PROBLEM: In its origin form, the problem was the conqueror's: converting the captured imperial cathedral into the victor's imperial mosque and binding it perpetually to his pious endowment. In its modern form, the arrangement answers the problem, as this reading frames it, of an illegitimate 1934 suspension — restoring worship on a site whose endowment deed, on this account, never authorized museum conversion.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is partial and asymmetric. Academic scholarship on Ottoman endowment law corroborates the existence and general terms of the 1453 endowment instrument, but no attestation from outside the beneficiary set affirms that those terms impose a present-day restoration obligation: secularist jurists, international heritage-law scholars, and the Ecumenical Patriarchate expressly dispute the perpetual-obligation reading, and the judicial validation came from chambers inside the state apparatus whose legitimacy narrative the arrangement serves.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the arrangement converts a shared symbolic asset into concentrated political capital, imposes access costs on defined payer seats, and redirects a fee stream, while a genuine worship function runs underneath. Suppression is 0.58 — unscaled raw structure — reflecting legal-administrative closure of alternatives (rival readings barred from the deciding process, court validation secured, heritage consultation declined, access policed) rather than overt violence. Theater_ratio 0.3: the worship is real and continuous, but a measurable share of activity around the site is conquest-anniversary pageantry and base-mobilization messaging. Accessibility_collapse 0.68: within Turkish jurisdiction the alternatives (museum status, shared-use regimes, restitution) are closed by decree and judgment; internationally the contest persists, keeping the value short of natural-law levels. Resistance 0.5: substantial and repeated (litigation, protests, diplomatic objection, heritage-regime censure) but unsuccessful. Claimed_type tangled_rope is stated independently of these scalars: the structure exhibits a genuine coordination function (unified custody solving an allocation problem for a singular contested sacred space), asymmetric extraction through the same structure (consolidation rents, fee streams, access priority), and active enforcement (executive decree plus judicial validation plus operational policing of access) — the canonical tangled_rope triple. The measurement series run on one shared grid (t in years since 1934; points 0, 20, 40, 50, 60, 70, 80, 85, 86, 90) with every tracked metric authored at every point. The series show a spike-and-decay cycle rather than monotone drift: extraction and theater peak sharply at the reconversion moment (t=86, 2020 — an electorally timed ceremonial maximum) and settle to a steady state by 2024, while the suppression requirement steps up at reconversion and holds, marking durable enforcement-infrastructure buildup. Part of the oscillation is the mechanism itself: anniversary-cycle mobilization is intermittent reinforcement of the coalition's base, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes materially different types across seats from this single structural surface. From the worshipper seat the arrangement is experienced as restored right and ordinary religious coordination; from the agenda seats as discharge of an endowment obligation; from the secularist seat as defeat of the settlement their civic identity is constituted through — identity-locked, so their effective extraction sits near the full-target end despite their organized power; from the patriarchate seat as closure of a restitution path it cannot stop pressing and cannot escape; from the heritage-regime seat as jurisdictional dispossession with no enforcement lever; from the visitor seat as friction substantially softened by substitutability. Coalition potential among payers is real but leveraged poorly: the domestic organized payer (secularist constituency) already exhausted the judicial route inside a captured validating institution, and the cross-border coalition (Greek state, Orthodox communion, heritage regime) holds only reputational instruments that the sovereignty premise explicitly designates as external interference.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-directionality end: the coalition and the directorate author and administer the arrangement and collect its consolidation and custodial gains; the worship constituency receives additive access it does not depend on exclusively; the global community receives a purely symbolic flow with trivially mobile attention. Payers cluster at the high end with exit-modulated spread: secularist Turks (identity_locked) and the patriarchate (trapped) sit nearest full-target because they cannot leave the symbolic and canonical order the arrangement governs; the heritage regime (constrained) sits near-full-target institutionally; non-Muslim visitors (mobile) bear real but substitution-dampened costs. The two agenda-setting state organs without direct beneficiary declarations (presidency, court chamber) derive their directionality from their enforcement role in the derivation chain — they hold the structure open for the seats that collect. Suppression is authored as a raw structural property and enters computation unscaled; only extractiveness is scaled by directionality and spatial scope, which is why the national-scoped identity conflict amplifies differently for the identity-locked domestic payer than for the mobile visitor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is authored as live within this reading: an endowment obligation framed as perpetual does not sunset, and no sunset clause exists. The R5 mismatch check therefore returns consistent (live status x world-rearranges verdict), with no zombie flag — the arrangement persists because its beneficiaries maintain it, not because anyone performs a dead function. The tangled_rope classification guards both mislabels: reading the arrangement as pure snare erases the genuine coordination it performs (continuous congregational worship, unified conservation and access management for a singular contested structure — millions pray here daily, and that function is not cover), while reading it as pure rope erases the asymmetric extraction running through the same structure (consolidation rents accruing to the governing coalition, fee streams to the religious directorate, access and jurisdictional costs pushed onto payers with weak or absent levers). Keeping both components visible is what makes the per-seat divergence legible rather than averaging it away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hagia_substrate_kernel_reading_position,
    'This constraint instantiates the islamic_sovereignty_reading of kernel hagia_sophia_substrate. What structural data would the sibling readings (universal_heritage_reading, orthodox_restitution_reading) produce for the same physical site?',
    'Generate each sibling as its own constraint story with its own epsilon, beneficiary/victim surface, and claimed type; compare computed classifications across the family. Under the universal reading the beneficiary set shifts toward the global public and the heritage regime and the arrangement''s exclusivity itself becomes the harm (low-extraction, rope-leaning profile); under the orthodox restitution reading the patriarchate becomes beneficiary and the Turkish state apparatus becomes extractor (snare-leaning from that seat).',
    'Cross-reading comparison is the point of the kernel: the same stones instantiate different constraints with different epsilon depending on which legitimacy premise governs. Any meta-analysis pooling the readings as one constraint destroys the signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hagia_substrate_kernel_reading_position, conceptual, 'Committer position: this story is one reading of a three-way contested kernel; siblings are separate files.').

omega_variable(
    determinative_title_locus,
    'Where is the substantive disagreement located — which act fixes legitimate custody of the site: the 1453 conquest and its endowment deed, the sixth-century imperial consecration, or the site''s standing as transnational human patrimony?',
    'Not resolvable by evidence alone; each premise is internally coherent and selects its own evidentiary base (endowment-law records, canon law and founding history, or international heritage jurisprudence). Resolution, if any, comes through political and legal settlement of which framework governs.',
    'The location of the disagreement determines which sibling reading a party can adopt without abandoning its framework, and therefore which structural deltas are live for each constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinative_title_locus, conceptual, 'The kernel contest reduces to a determinative-title question among three candidate legitimating acts.').

omega_variable(
    title_vs_layered_sanctity_framing,
    'Is the correct framing strict-title (exactly one legitimating act governs, so the conquest-endowment and founding-consecration premises are mutually exclusive) or layered-sanctity (frameworks can hold conquest-era title validity alongside persistent founding sanctity, as some canon-law reversion doctrines do)?',
    'Comparative analysis of how religious-law traditions themselves handle converted sacred buildings: if authoritative frameworks within the relevant traditions routinely hold both claims in layered form, the layered framing is the more defensible declaration.',
    'Under the layered framing, the forecloses relation asserted toward the orthodox restitution sibling weakens toward coexists_with, and more of the measured extraction reattributes from title exclusivity itself to the political consolidation riding on it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(title_vs_layered_sanctity_framing, conceptual, 'Framing under-determination: strict-title versus layered-sanctity framings yield different reading relations and different epsilon composition.').

omega_variable(
    waqf_obligation_scope,
    'Do the historical endowment deed''s terms, as a matter of Ottoman and Turkish endowment law, actually compel mosque restoration, or do they permit custodial flexibility that the 1934 conversion lawfully exercised?',
    'Independent endowment-law scholarship and archival analysis of the deed''s conditions, assessed apart from the interested litigation that produced the 2020 ruling.',
    'If the deed compels restoration, the arrangement''s coordination component is doctrinally compelled and its rope-side weighting rises; if the terms are permissive, the selection of restoration over continuation was political, and the extraction share attributable to discretionary choice increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_obligation_scope, empirical, 'Whether the coordination component is doctrinally compelled or politically selected.').

omega_variable(
    secularist_accommodation_trajectory,
    'Will the secularist payer seat accommodate to the restored arrangement over time, or does the identity-locked character of its position guarantee recurring mobilization?',
    'Longitudinal observation of opposition intensity across electoral cycles: turnout, litigation frequency, anniversary-protest scale, and generational replacement within the secularist constituency.',
    'Accommodation lowers the resistance trajectory and stabilizes the tangled_rope configuration; renewed mobilization raises the suppression requirement further and pushes the arrangement''s enforcement dependence upward, drifting it toward snare-side operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularist_accommodation_trajectory, empirical, 'Persistence question: whether the principal domestic payer seat hardens or accommodates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hagi_tr_t40, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(hagi_tr_t50, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(hagi_tr_t70, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 70, 0.34).
narrative_ontology:measurement(hagi_tr_t80, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(hagi_tr_t85, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 85, 0.44).
narrative_ontology:measurement(hagi_tr_t86, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 86, 0.55).
narrative_ontology:measurement(hagi_tr_t90, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 90, 0.3).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(hagi_be_t40, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(hagi_be_t50, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 50, 0.34).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(hagi_be_t70, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 70, 0.47).
narrative_ontology:measurement(hagi_be_t80, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(hagi_be_t85, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 85, 0.56).
narrative_ontology:measurement(hagi_be_t86, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 86, 0.78).
narrative_ontology:measurement(hagi_be_t90, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 90, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(hagi_su_t20, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(hagi_su_t40, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement(hagi_su_t50, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 60, 0.23).
narrative_ontology:measurement(hagi_su_t70, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 70, 0.27).
narrative_ontology:measurement(hagi_su_t80, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(hagi_su_t85, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 85, 0.34).
narrative_ontology:measurement(hagi_su_t86, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 86, 0.57).
narrative_ontology:measurement(hagi_su_t90, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 90, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the status of Hagia Sophia.' The single label conflates three structurally distinct claims: a sovereignty-title claim (this file — legitimacy flows from the 1453 conquest-endowment under Turkish state custody), a founding-ecclesial claim (orthodox_restitution_reading — legitimacy flows from the cathedral founding and demands return or neutrality), and a patrimony-commons claim (universal_heritage_reading — legitimacy flows from transnational human heritage standing). Each is authored as its own file with its own epsilon over the same physical referent, per the epsilon-invariance principle: measuring the standing arrangement through different legitimacy lenses yields different beneficiary/victim structures and different extraction profiles, which is the signature of distinct constraints sharing a substrate, not one observable-dependent constraint. The universal-heritage reading is upstream (its institutional vehicle, the World Heritage inscription, predates the reconversion contest and shapes the environment in which both other readings operate); this reading exerts structural pressure on both siblings by reasserting exclusive title, degrading the heritage regime's jurisdiction and closing the restitution path without logically eliminating either sibling's claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
