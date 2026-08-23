% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Regime (Enforced Kami-Buddha Identity)
 *   domain: religious/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   From roughly the ninth century onward, the great esoteric Buddhist houses
 *   taught that Japan's kami are not rival gods but local manifestations
 *   ('traces') of universal buddhas ('original grounds'): Amaterasu as a
 *   trace of Dainichi, Hachiman as a bodhisattva figure. This story models
 *   the standing arrangement that enforced that teaching: resident chapels at
 *   shrines, ordination of shrine clergy, doctrinal licensing of what could
 *   be said about kami identity, and shrine finance routed through temple
 *   administration. The claim/metric split is deliberate: the reading CLAIMS
 *   the fusion is metaphysical truth making the arrangement a faithful
 *   ordering of reality, while the authored METRICS describe an actively
 *   enforced arrangement with substantial asymmetric transfer - the engine
 *   measures that divergence rather than the author resolving it. Assumptions
 *   stated: proto-forms of the doctrine appear in Nara-period texts; the
 *   systematic regime dated here begins with tenth-century esoteric
 *   systematization; the interval ends at 1860, immediately before the 1868
 *   separation edicts that lie outside this story's window. This file is one
 *   member of the three-story simultaneous_veneration family; see
 *   network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishments: agenda-setter and principal collector ([institutional]/[arbitrage]) - authors the doctrine, administers shrine chapels, defines orthodoxy, receives ordination fees, votive income, and estate authority
 *   - imperial_court: secondary beneficiary ([powerful]/[arbitrage]) - collects ritual-political integration across the realm, shifts patronage freely
 *   - shrine_priest_lineages: primary target ([moderate]/[identity_locked]) - hereditary kami custodians whose office, kinship, and self-conception are fused with the offices being subordinated
 *   - village_devotee_communities: dual-positioned mass seat ([powerless]/[constrained]) - receives the integrated ritual calendar, carries registration and offering burdens
 *   - reverse_theory_shrine_reformers: excluded challenger seat ([moderate]/[identity_locked]) - develops inverted doctrine from inside shrine office, unseated in orthodox councils
 *   - meiji_separation_authorities: analytical observer ([institutional]/[analytical]) - maps the whole structure at the interval's edge in order to dismantle it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.7).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.5).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Regime (Enforced Kami-Buddha Identity)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '25cb5886-b0e6-45ad-ab6e-0cc789861663').
narrative_ontology:cs_kernel_codification('25cb5886-b0e6-45ad-ab6e-0cc789861663', fixed_text).
narrative_ontology:cs_authority_grounding('25cb5886-b0e6-45ad-ab6e-0cc789861663', lineage).
narrative_ontology:cs_interpretation_layer_present('25cb5886-b0e6-45ad-ab6e-0cc789861663').
narrative_ontology:cs_reading_relation('25cb5886-b0e6-45ad-ab6e-0cc789861663', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('25cb5886-b0e6-45ad-ab6e-0cc789861663', simultaneous_veneration__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('25cb5886-b0e6-45ad-ab6e-0cc789861663', foundational, kami_are_manifest_traces_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_manifest_traces_of_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('25cb5886-b0e6-45ad-ab6e-0cc789861663', kami_are_manifest_traces_of_buddhas, theological).
narrative_ontology:cs_axiom('25cb5886-b0e6-45ad-ab6e-0cc789861663', secondary, kami_identity_requires_authorized_transmission).
narrative_ontology:cs_axiom_status(kami_identity_requires_authorized_transmission, holdable).
narrative_ontology:cs_axiom_grounding('25cb5886-b0e6-45ad-ab6e-0cc789861663', kami_identity_requires_authorized_transmission, conventional).
narrative_ontology:cs_reference_frame('25cb5886-b0e6-45ad-ab6e-0cc789861663', honji_suijaku_metaphysical_identity).
narrative_ontology:cs_drift_state('25cb5886-b0e6-45ad-ab6e-0cc789861663', meiji_separation_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('25cb5886-b0e6-45ad-ab6e-0cc789861663', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, imperial_court).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, village_devotee_communities).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shrine_priest_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, village_devotee_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The esoteric Tendai and Shingon houses (Enryakuji, Onjoji, Daigoji and their branch networks) formulate the fusion doctrine through initiation lineages, install resident chapels at major shrines, ordain shrine clergy into Buddhist ranks, and adjudicate which accounts of kami identity count as orthodox through commentary and secret transmission documents. Votive income, ordination fees, and shrine-estate administration flow to their complexes. They wrote the framework and sit at its center; their exposure is doctrinal reputation rather than livelihood, and they can relocate personnel and patronage among their own branches at will.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments, beneficiary).

% Patronizes the synthesis because it binds thousands of provincial cults into a single ritual order legible under imperial ceremony: reign-legitimating rites, festival calendars, and prayer services all route through the unified shrine-temple complex. Grants temple-shrine estates and adjudicates disputes between them. Collects the integration dividend without running doctrine day-to-day, and can shift favor between particular temples and shrines when one house grows troublesome.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, imperial_court, beneficiary,
    powerful, generational, arbitrage, national).

% Hereditary custodial clans (Nakatomi, Imbe, Watarai, Urabe, and hundreds of provincial priestly houses) whose office, marriage alliances, and self-understanding are fused with service to a particular kami. Under the fusion arrangement their deity is defined as a trace of a buddha, their ancestral rites are reframed as provisional expedients, sons take Buddhist ordination to advance, and shrine revenues pass through resident chapels they do not control. Leaving means abandoning a lineage obligation that constitutes who they are; remaining means accepting subordinate definition. Most comply and negotiate, a few theorize resistance.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shrine_priest_lineages, payer,
    moderate, generational, identity_locked, national).

% Farmers and townspeople who receive an integrated ritual life on one coherent calendar: festivals, healing rites, harvest ceremonies, and funerary salvation that make kami shrines and Buddhist temples mutually intelligible. They pay for it through offerings, later through compulsory temple registration and certification obligations binding households to parish temples, and occasionally through corvee for chapel construction. Participation is compulsory in effect since burial, registration, and communal standing all route through the shrine-temple complex, but the practices received are genuinely valued.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, village_devotee_communities, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, village_devotee_communities, payer).

% Ise Watarai priests from the thirteenth century and later the Yoshida house develop inverted doctrines (kami as the originals, buddhas as their traces) and pure-kami theologies rejecting the fusion premise outright. Their texts circulate at the margins, are periodically condemned or absorbed under license issued by the very institutions they challenge, and their authors remain shrine men bound to the offices they defend. They would speak in the doctrinal councils that defined orthodoxy but were never seated there; their influence arrived only later and on the establishment's terms.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, reverse_theory_shrine_reformers, excluded,
    moderate, generational, identity_locked, regional).

% The Meiji Council of State and its nativist ideologues study the arrangement's full history in order to dismantle it: the 1868 separation edicts sever kami-buddha fusion by administrative fiat, evict Buddhist clergy from shrines, and redefine kami veneration as unborrowed native practice. An analytical seat at the interval's edge that sees the whole structure because its project requires seeing it whole; it bears none of the arrangement's ongoing costs and collects none of its benefits.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_separation_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved the collision of two parallel cult economies: gave indigenous kami salvific standing inside the Buddhist cosmos, standardized ritual calendars and clerical training across thousands of shrine sites, and made kami festivals and Buddhist funerary rites mutually intelligible for the same devotees, so that honoring one's village deity and securing one's afterlife no longer pulled in opposite directions.
% TRANSFER_FUNCTION: Moved interpretive authority over kami identity, ordination rights, votive income, and shrine-estate administration from hereditary shrine lineages and local cult communities up to the great monastic corporations and their court patrons; moved devotional labor, offerings, and later compulsory registration compliance upward through the shrine-temple hierarchy.
% ABSENT_VOICES: Shrine-priest lineages that never accepted subordinate definition and pure-kami theorists had no seat in the doctrinal councils convened by monastic academies where orthodoxy was settled; the kami themselves, as their devotees understood them, spoke only through a doctrine claiming authority to reinterpret them. Dissent existed (Watarai, Yoshida, later kokugaku scholarship) but entered the record filtered through licensing or condemnation.
% DISAPPEARANCE_RATIONALE: When the state severed the fusion in 1868 the religious landscape rearranged wholesale: thousands of shrine chapels were demolished or stripped, Buddhist clergy at shrines were laicized or expelled, shrine and temple finance were separated, kami were redefined as unborrowed native deities, and a new state cult was erected on the cleared ground. Nothing about Japanese religious organization remained in place.
% FOUNDING_PROBLEM: How could imported Buddhist soteriology and indigenous kami cults coexist, given that Buddhist cosmology classified non-Buddhist beings as deluded and unsaved: could kami be honored within a Buddhist universe, and could devotees serve both without contradiction?
% FOUNDING_PROBLEM_CORROBORATION: Court chronicles, shrine-temple dispute records, and aristocratic diaries corroborate that the integration problem was pressing and real from the tenth century onward. Kokugaku scholars (Motoori Norinaga, Hirata Atsutane) and Watarai and Yoshida polemicists attest from outside the benefiting parties that the problem had been solved, or was never genuine, centuries before the arrangement's end; the Meiji edicts constitute a further third-party judgment that the founding problem no longer justified the arrangement. After the seventeenth century no attestation of continued liveness survives from any seat outside the Buddhist establishment itself.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70 at interval end) because the arrangement transfers interpretive authority and shrine finance to parties who did not originally hold them, and the transfer is decoupled from any service the shrines could not otherwise obtain. Suppression is moderate (0.50): enforcement was real and grew - chapel installation, ordination requirements, Tokugawa-era household registration binding devotees to temples - but never totalized; pure-kami lineages and inverted doctrines survived at the margins throughout, so the standing arrangement's end-state suppressive force is below its historical peak. Theater crosses 0.5 around 1700: by the early modern period the integration problem was long solved, and a growing share of doctrinal production defended the monopoly and elaborated combinatory liturgy rather than performed integration - a Goodhart-drift signature. Accessibility_collapse is 0.55: alternatives narrowed sharply but never closed. Resistance is 0.55: sustained intellectual resistance ran eight centuries (Watarai, Yoshida, kokugaku) alongside episodic litigation. All three series share one time grid; suppression_requirement is tracked temporally because this story specifically traces enforcement-capacity change (build-up through Kamakura-Muromachi, peak under Tokugawa registration, decay before Meiji), not merely shifting extraction. The late-interval dissociation - suppression falling while extraction holds flat - is the load-bearing finding: extraction persisted on institutional momentum after its enforcement rationale weakened, which feeds the mandatrophy analysis below.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the monastic seat the arrangement is a truth faithfully administered: the fusion doctrine is simply what the cosmos is like, and administering shrines is stewardship, not taking. From the identity-locked shrine-priest seat the same structure is subordination of ancestral office - and lock-in amplifies the burden, since exit means dissolving the lineage self. The villager seat sits near symmetry: genuine integrated ritual life against compulsory registration costs. The court seat collects integration dividends with portfolio mobility. The excluded reformer seat sees the structure most clearly precisely because it was never seated in it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (monastic establishments, court, devotee villages) derive low directionality; the victim group (shrine priest lineages) derives high directionality, pushed toward the full-target end by identity-locked exit. One override is declared: the powerless seat (village_devotee_communities) is overridden to d=0.38 because the automatic derivation reads its primary beneficiary declaration and would place it deep in subsidized territory, while the situation carries real compulsory burdens (registration, offerings, corvee) that place it near symmetric - a genuinely dual-positioned mass seat. Scope is national, which modestly amplifies effective extraction through verification difficulty (engine-owned arithmetic). No other overrides are needed: role plus exit options determine every remaining seat without distortion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconciling kami cults with Buddhist soteriology - was effectively solved by the late medieval period, after which the arrangement's maintenance shifted toward defending interpretive territory: theater_ratio exceeds 0.5 from 1700 onward, the classic signature of proxy maintenance replacing function. No sunset clause was ever authored; the mandate was terminated externally by the 1868 edicts rather than internally retired. Claiming tangled_rope prevents both mislabels: a pure-snare reading would erase the genuine eight-century coordination achievement that made the synthesis stick (integrated calendars, salvific standing for kami, mutual legibility of shrine and temple practice); a pure-rope reading would erase the concentrated capture of kami identity and shrine finance by the monastic corporations. The classification keeps both halves visible, and the late-interval measurement dissociation (falling suppression, flat extraction, rising theater) marks exactly where the arrangement slid toward vestige before the rupture finished what inertia had begun.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_fusion_truth_status,
    'Is the ontological fusion of kami and buddhas a metaphysical fact (as this reading asserts), or one rival reading among at least three of the same veneration practice?',
    'Internal doctrinal criteria cannot decide the question; resolution requires cross-reading structural comparison - which reading better predicts the historical enforcement patterns, the absorption of dissent, and the post-1868 persistence of kami veneration - reviewed against historian-of-religions consensus outside any benefiting party.',
    'If fusion is one contingent reading rather than truth, this story''s epsilon measures the enforcement of an interpretation rather than the defense of reality, strengthening extraction-side classification and validating the sibling readings as co-equal constraints rather than errors to be corrected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_fusion_truth_status, conceptual, 'Committer-frame omega: this constraint is the ontological_fusion_reading of kernel simultaneous_veneration; sibling readings (domain_partition, pragmatic_incoherence) would restructure beneficiary and victim sets entirely.').

omega_variable(
    monopoly_rent_vs_coordination_cost,
    'What share of the measured extraction is interpretive-monopoly capture by the monastic corporations, versus the irreducible cost of integrating two cult systems into one ritual economy?',
    'Comparative shrine-economics analysis across periods when counter-doctrines held licenses (Watarai and Yoshida intervals) and post-1868 shrine-autonomy records, isolating what shrine finance and clerical authority look like when fusion enforcement relaxes.',
    'A majority-monopoly share pushes classification toward snare; a majority-coordination-cost share pushes toward rope and would reframe the arrangement''s persistence as priced coordination rather than capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_rent_vs_coordination_cost, empirical, 'Decomposition of the extraction component into capture versus genuine integration overhead.').

omega_variable(
    reverse_reading_absorption_ambiguity,
    'Were the licensed counter-doctrines (Watarai, Yoshida) genuine alternative channels for kami autonomy, or safety valves whose licensing stabilized the fusion order they appeared to oppose?',
    'Track substantive autonomy outcomes for licensed houses versus unlicensed shrines: control of revenue, ordination independence, doctrinal publication rights. Genuine channels widen autonomy; valves narrow it while performing opposition.',
    'If safety valve, suppression is deeper than the scalar suggests and accessibility_collapse should be revised upward; if genuine alternative, the authored values stand and resistance met less suppression than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_reading_absorption_ambiguity, empirical, 'Whether licensed dissent functioned as release or as reinforcement of the orthodoxy it challenged.').

omega_variable(
    persistence_without_enforcement_counterfactual,
    'At the interval''s end, would the fusion arrangement have persisted absent state coercion, or was it already sustained only by inertia and theatrical maintenance?',
    'Analyze enforcement-cost trends and voluntary compliance rates from 1750 to 1868, including regional comparisons where Tokugawa enforcement capacity lapsed earliest.',
    'Inertial persistence confirms a late-interval drift toward vestige character before the external rupture; self-sustaining persistence would indicate the coordination function remained live at dissolution, sharpening the loss attributable to the 1868 severance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_without_enforcement_counterfactual, empirical, 'Counterfactual persistence question bearing on the arrangement''s condition immediately before the Meiji rupture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 850, 1860).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1100, 0.19).
narrative_ontology:measurement(simu_tr_t1250, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1250, 0.27).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(simu_tr_t1550, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1550, 0.43).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1700, 0.52).
narrative_ontology:measurement(simu_tr_t1785, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1785, 0.56).
narrative_ontology:measurement(simu_tr_t1860, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1860, 0.58).

% Extraction over time
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.46).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1100, 0.57).
narrative_ontology:measurement(simu_be_t1250, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1250, 0.65).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1400, 0.69).
narrative_ontology:measurement(simu_be_t1550, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1550, 0.7).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1700, 0.71).
narrative_ontology:measurement(simu_be_t1785, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1785, 0.71).
narrative_ontology:measurement(simu_be_t1860, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1860, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.28).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1100, 0.45).
narrative_ontology:measurement(simu_su_t1250, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1250, 0.58).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1400, 0.63).
narrative_ontology:measurement(simu_su_t1550, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1550, 0.6).
narrative_ontology:measurement(simu_su_t1700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1700, 0.67).
narrative_ontology:measurement(simu_su_t1785, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1785, 0.6).
narrative_ontology:measurement(simu_su_t1860, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1860, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'simultaneous veneration of kami and buddhas' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing the kernel 'simultaneous_veneration': this ontological_fusion_reading (enforced identity, high epsilon, tangled_rope profile); domain_partition_reading (functional specialization of distinct beings, lower epsilon, coordination-dominant profile); and pragmatic_incoherence_reading (unresolved contradiction sustained by absent enforcement, retrospective analytic profile). This file authors epsilon only for the fusion-enforcement arrangement as the fusion reading frames it. The partition reading is upstream historically (functional coexistence predates systematic fusion doctrine and is cited by it as a stage), and the incoherence reading is downstream retrospectively (its thesis presupposes the enforcement apparatus this reading describes having lapsed). Each sibling file carries its own beneficiaries, victims, and classification; the edges here are family membership links, not causal endorsements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__ontological_fusion_reading, powerless, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
