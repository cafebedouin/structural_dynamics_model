% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita — Universalist Devotional Reading (Birth-Independent Bhakti)
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The universalist devotional reading holds the Bhagavad Gita to teach a
 *   path of loving devotion (bhakti) open to every person regardless of
 *   caste, and to redefine dharma as surrender to divine will rather than
 *   performance of birth-assigned social role. As a standing arrangement, the
 *   reading governs how devotional communities form, who may teach, and what
 *   access to liberation requires: no ritual mediation, no birth
 *   qualification. Its operation subsidizes devotees broadly, displaces the
 *   mediating position of hereditary ritual specialists, and sustains a
 *   modest institutional economy of preceptor lineages. This story is ONE
 *   reading of the gita_kurukshetra_discourse kernel; the orthodox literal
 *   and Gandhian allegorical readings are separate constraint files with
 *   their own epsilon values and victim sets, linked through the network
 *   block. Per the epsilon-referent rule, epsilon here is authored for the
 *   standing arrangement this reading constitutes, assessed by the reading's
 *   own lights — not for the arrangements its siblings would endorse. KEY
 *   AGENTS (by structural relationship): - universal_devotee_class: primary
 *   beneficiary (organized/mobile) — cross-caste practitioners gaining
 *   birth-independent access - marginalized_caste_devotees: concentrated
 *   beneficiary (powerless/identity_locked) — devotees from ritually excluded
 *   castes for whom the congregation carries dignity -
 *   brahminical_gatekeeper_priesthood: principal cost-bearer
 *   (institutional/arbitrage) — hereditary specialists losing mediating
 *   standing - acharya_lineage_heads: administrator and modest collector
 *   (institutional/arbitrage) — transmit the reading and decide its
 *   boundaries - ambedkarite_dalit_critics: excluded voice (organized/mobile)
 *   — object that devotional consolation blunts material struggle -
 *   comparative_religion_scholars: analytical observer
 *   (analytical/analytical) — date the layers, trace the genealogy
 *
 * KEY AGENTS:
 *   - universal_devotee_class: primary beneficiary (organized/mobile) — gains birth-independent access and a portable religious identity
 *   - marginalized_caste_devotees: concentrated beneficiary (powerless/identity_locked) — the reading's leveling effect lands hardest and matters most here
 *   - brahminical_gatekeeper_priesthood: principal cost-bearer (institutional/arbitrage) — loses mediating rents; repositions rather than absorbs
 *   - acharya_lineage_heads: agenda-setter with secondary beneficiary position (institutional/arbitrage) — administers transmission, collects offerings
 *   - ambedkarite_dalit_critics: excluded voice (organized/mobile) — contest the reading's consoling function from outside its circuits
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — establish the composite-text facts both sides select from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.3).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.19).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita — Universalist Devotional Reading (Birth-Independent Bhakti)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '87b11d66-0718-428a-b3ed-acbbd05abdd5').
narrative_ontology:cs_kernel_codification('87b11d66-0718-428a-b3ed-acbbd05abdd5', fixed_text).
narrative_ontology:cs_authority_grounding('87b11d66-0718-428a-b3ed-acbbd05abdd5', lineage).
narrative_ontology:cs_interpretation_layer_present('87b11d66-0718-428a-b3ed-acbbd05abdd5').
narrative_ontology:cs_reading_relation('87b11d66-0718-428a-b3ed-acbbd05abdd5', gita_kurukshetra_discourse__orthodox_literal_reading, influences).
narrative_ontology:cs_reading_relation('87b11d66-0718-428a-b3ed-acbbd05abdd5', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_axiom('87b11d66-0718-428a-b3ed-acbbd05abdd5', foundational, birth_independent_salvific_access).
narrative_ontology:cs_axiom_status(birth_independent_salvific_access, holdable).
narrative_ontology:cs_axiom_grounding('87b11d66-0718-428a-b3ed-acbbd05abdd5', birth_independent_salvific_access, theological).
narrative_ontology:cs_axiom('87b11d66-0718-428a-b3ed-acbbd05abdd5', foundational, dharma_as_surrender_not_social_role).
narrative_ontology:cs_axiom_status(dharma_as_surrender_not_social_role, holdable).
narrative_ontology:cs_axiom_grounding('87b11d66-0718-428a-b3ed-acbbd05abdd5', dharma_as_surrender_not_social_role, theological).
narrative_ontology:cs_reference_frame('87b11d66-0718-428a-b3ed-acbbd05abdd5', unqualified_divine_refuge_promise).
narrative_ontology:cs_drift_state('87b11d66-0718-428a-b3ed-acbbd05abdd5', contemporary_global_devotional_economy, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('87b11d66-0718-428a-b3ed-acbbd05abdd5', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_caste_devotees).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeeper_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, acharya_lineage_heads).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners across every caste who take up the devotional path the reading describes: chanting, service, study, and offering of outcomes to the divine. What flows to them is salvific assurance and a religious identity that requires no birth qualification and no hereditary mediator; what flows from them is devotion, service labor, and offerings. Leaving is straightforward in principle — the reading itself affirms other yogas alongside devotion — and some practitioners do drift to other paths or to none.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    organized, biographical, mobile, global).

% Devotees from castes the older ritual order barred from Vedic access. The devotional congregation is often the first religious space that receives them without purification rites or separate seating, and for many it becomes the primary bearer of personal dignity against caste stigma. Departure would mean exiting that dignity-bearing belonging back into the stigmatized social world, so ties run deep even where particular congregations disappoint.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, marginalized_caste_devotees, beneficiary,
    powerless, biographical, identity_locked, continental).

% Hereditary ritual specialists whose traditional income and standing rested on mediating access to scripture and sacrament. As the reading spreads, patrons fund devotional singing, pilgrimage, and printing instead of exclusive rites, and the claim that birth qualifies one to mediate loses persuasive force. Many houses have repositioned rather than simply declined — leading devotional lineages themselves, composing harmonizing commentaries, or staffing temples that now host open congregations.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeeper_priesthood, payer,
    institutional, generational, arbitrage, continental).

% Teachers and lineage heads who transmit the reading, train devotees, and administer congregations, schools, and publishing operations. They decide which interpretations circulate under the reading's banner and collect the offerings, fees, and deference that sustain their institutions. Their position depends on continued credibility that the teaching is open to all; several lineages have faced public challenge when succession or fundraising looked closed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, acharya_lineage_heads, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, acharya_lineage_heads, beneficiary).

% Modern critics, drawing on Ambedkar's writings, who argue that consoling devotional framings blunt the struggle for material equality and that the text's composite layers still carry caste-apologetic material. They publish and organize outside the devotional and curricular circuits where the reading is canonized, and their objections rarely enter the congregational conversation the reading governs.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, ambedkarite_dalit_critics, excluded,
    organized, generational, mobile, national).

% Academic philologists and historians who date the text's compositional layers, trace the reading's commentarial genealogy, and document how living communities deploy it. They hold no stake in the reading's truth and can therefore say what partisans cannot: which verses carry the universalist promise, which affirm varna duty, and how emphasis has shifted across centuries.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, acharya_lineage_heads).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, birth-unqualified route to liberation and coordinates a cross-caste devotional public around shared practice — chanting, service, festival, study — that requires no hereditary mediator to conduct.
% TRANSFER_FUNCTION: Moves devotion, service labor, and offerings from devotees of all castes toward the divine as conceived and, materially, toward the preceptor lineages that transmit the teaching; moves salvific assurance, dignified religious identity, and textual literacy outward to devotees.
% ABSENT_VOICES: Ambedkarite and materialist critics would object that devotional consolation blunts the fight for material equality; they sit outside the congregational and curricular circuits where the reading is canonized. Orthodox ritualists, sidelined in modern global dissemination, would object that the reading strips the text of its social architecture. Neither is seated in the devotional conversation this reading governs.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, hundreds of millions of practitioners would lose the access-framing that organizes their practice, vernacular and global devotional traditions built on it would lose their warrant, preceptor lineages would lose their charter, and birth-qualified gatekeeping claims would resurface to fill the vacancy — the religious economy of the text would reorganize around whichever reading seized the open slot.
% FOUNDING_PROBLEM: Salvific access appeared rationed by birth: the ritual order reserved Vedic recitation and sacramental mediation for the twice-born, leaving most people dependent on intermediaries or excluded outright. The reading was formed — in the text's own universalist verses and in centuries of commentarial and vernacular deployment — to open the path to anyone who takes refuge, regardless of caste.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Dalit-rights scholarship and Ambedkarite writing document continuing ritual and congregational exclusion; historians of the bhakti movements attest both the reading's anti-gatekeeping genealogy and the persistence of the exclusion it targeted; the sustained resistance of orthodox opponents itself confirms that the access claim threatens a standing gatekeeping interest. No party inside the arrangement is the source of the status claim.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is rope, stated from the reading's own lights: the arrangement solves a genuine collective problem (salvific access rationed by birth), its participants are net beneficiaries, its coercive overhead is minimal, and it does not suppress alternatives — on this reading the text explicitly leaves karma and jnana yogas standing alongside bhakti. The metrics are authored independently as descriptive facts, not reconciled to the claim: extractiveness 0.30 reflects the modest but real flows that remain — offerings, fees, and deference toward preceptor lineages, plus the discipline cost of surrender itself — well above zero but far below gatekeeping-era levels; suppression 0.19 reflects soft communal sanction rather than enforcement machinery (suppression is authored as a raw structural property; the engine, not this story, scales extractiveness by directionality and scope). Theater 0.25 tracks the media-age growth of devotional spectacle around otherwise-live practice. Accessibility collapse is low (0.35) because understanding the arrangement does not close alternatives — plural paths remain explicit; resistance is moderate (0.50): orthodox establishments resisted the reading historically and Ambedkarite critique contests it now. The measurement series share one six-point grid (t=0..100, approximately 1925–2025); all three tracked series rise gently, modeling institutional consolidation — reformist universalism, then mass post-independence movements, then global guru organizations with media spectacle — rather than decay or oscillation; no cyclical dynamics are claimed. Fixing (dismantling) the arrangement is prohibitive relative to any benefit: it is load-bearing for community identity across castes, and no seat could remove it without catastrophic loss, hence fixing_cost. Coordination type is identity_coordination: the arrangement's core work is maintaining an open-membership devotional identity — 'devotee' unqualified by birth — against caste-particularized membership norms, and its characteristic failure mode is re-closure of that boundary. The conservative floor for this type is left in place deliberately: the known gaming risk of identity framing covering extraction is answered by the measured extraction sitting near the floor, not by raising the floor.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the gatekeeper seat the reading operates as dispossession of a legitimate mediating office — high experienced imposition despite the arrangement's low aggregate extraction. From the devotee seats the same arrangement is emancipation: access formerly priced by birth arrives unpriced. From the acharya seat it is stewardship with modest collection — authority exercised to keep the door open, revenue taken to keep institutions alive. The scholar seat sees a composite text whose emphasis is selectable, which is why the same verses ground opposed readings. The engine computes these divergences from power, exit, and directional data; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. universal_devotee_class and marginalized_caste_devotees sit at the beneficiary end: the arrangement subsidizes them with access and identity, and their exits — other paths, simple departure — are real if sometimes socially costly. marginalized_caste_devotees carry an identity-lock modulation worth stating precisely: devotional belonging constitutes their counter-identity to caste stigma (a relational-ideological fusion), so the lock binds them to the BENEFIT, keeping their effective subsidy durable; were the identity frame to break — a congregation turning discriminatory, a lineage closing its succession — their d would rise sharply and the subsidy reading would fail for that seat. brahminical_gatekeeper_priesthood is the declared victim; sole-victim status alone would drive its derived d toward the full-target end, but its authored arbitrage exit (repositioning into devotional lineages, harmonizing commentary, temple staffing) modulates the derivation downward, since agents with arbitrage-grade exit sit away from the trapped-target pole. No directionality override is authored: the derivation chain already differentiates the two institutional seats correctly through their opposed role declarations combined with the same arbitrage exit, and an override keyed to the institutional power atom would wrongly move both seats together. acharya_lineage_heads derive low d from their beneficiary position; their administrative burden offsets collection only slightly, and the gain_flow field records that the arrangement's material gains demonstrably accrue to their seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — salvific access rationed by birth — remains live, corroborated from outside the benefiting parties, so no mandatrophy is declared. The classification disciplines two mislabels. Against the snare reading: displacing gatekeepers is not victimizing the coordinated — no exit is suppressed, no coercion sustains participation, and the displaced retain working arbitrage. Against the mountain reading: the arrangement is not a natural feature of the text; it is transmitted, administered, and contested, and it would not persist unattended. The live risk runs the other way — institutional extraction drift (see omega institutional_extraction_drift) could thicken lineage economies until the rope tangles; the measurement series exists to catch that transition, and the gentle rise in all three tracked series is the early signature worth watching.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the gita_kurukshetra_discourse kernel; the orthodox_literal_reading and gandhian_allegorical_reading instantiate different constraints over the same verses — where exactly is the disagreement located, and what would change structurally if a sibling reading governed instead?',
    'Compile the sibling stories and compare structural deltas: the divergence sits in whether the controlling teaching binds caste-social role or prescribes surrender to divine will, and whether the martial passages carry normative weight.',
    'Sibling governance flips the victim set (the orthodox reading declares no gatekeeper victims and suppresses dissenters; this reading''s victim is the displaced gatekeeper) and moves epsilon substantially; any kernel-level verdict is undefined until the three files are compared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, divergence located in the referent of dharma and the status of the violence passages.').

omega_variable(
    institutional_extraction_drift,
    'Does the reading''s low extraction survive institutionalization, or do preceptor-lineage economies reintroduce gatekeeping under universalist cover?',
    'Track offering burdens, succession openness, and entry costs across major lineages over time, and compare against the measurement series in this file.',
    'Drift past roughly 0.45 extractiveness would support reclassification toward tangled_rope; stabilization near current levels supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_drift, empirical, 'Whether universalist cover holds as lineage institutions thicken.').

omega_variable(
    surrender_agency_ambiguity,
    'Does the discipline of surrender to divine will release devotees from oppressive role obligations, or dampen resistance to material injustice?',
    'Comparative study of justice-seeking behavior — legal mobilization, collective bargaining, protest participation — between matched devotional and non-devotional populations within the same castes.',
    'If surrender measurably pacifies, effective imposition on marginalized devotees rises and the rope claim weakens toward tangled_rope; if it releases, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surrender_agency_ambiguity, conceptual, 'The Ambedkarite objection formalized: the agency effect of the surrender discipline.').

omega_variable(
    textual_stratum_weighting,
    'The text is a composite whose universalist verses (the refuge promise, the ''even those of humble birth'' verse) coexist with varna-affirming strata — is the reading''s low epsilon operative, or does it depend on which strata living communities treat as controlling?',
    'Reception-history philology: which verses are preached, printed, and memorized in living communities, weighted by congregational centrality.',
    'If varna strata control practice while universalist verses decorate, the arrangement''s real epsilon is higher than authored and the reading functions aspirationally rather than descriptively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stratum_weighting, empirical, 'Epsilon invariance across compositional strata and liturgical emphasis.').

omega_variable(
    spiritual_vs_social_caste_dissolution,
    'Does the reading dissolve caste as a spiritual barrier only, leaving social caste — marriage, occupation, congregation office — intact?',
    'Demographic comparison of endogamy rates, occupational distribution, and leadership composition inside major devotional communities versus surrounding populations.',
    'If social caste persists untouched inside devotional communities, the subsidy to marginalized devotees is smaller than modeled and the coordination function is partial rather than general.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_vs_social_caste_dissolution, empirical, 'Depth of the anti-gatekeeping effect: ritual equality versus social equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 80, 0.23).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 80, 0.29).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.13).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 80, 0.17).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'what the Gita teaches' — covers three structurally distinct constraints. The kernel gita_kurukshetra_discourse decomposes into: this universalist devotional reading (low epsilon, beneficiaries = devotee classes, victim = displaced gatekeepers, claimed rope); the orthodox literal reading (caste-duty mandate, war legitimated; its own epsilon and victim set); and the Gandhian allegorical reading (battlefield interiorized; its own epsilon). Each is a separate file with a single stable epsilon, per the epsilon-invariance principle. They are linked here because this reading's spread changes the operating environment of both siblings — eroding gatekeeping rents behind the orthodox reading and supplying the devotional universalism the allegorical reading presupposes — without logically eliminating either, since harmonizing two-level frameworks keep both siblings' core premises holdable within a single sophisticated framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
