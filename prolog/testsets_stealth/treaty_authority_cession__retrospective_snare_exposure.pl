% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Textual Divergence as Extraction Mechanism (Retrospective Snare Exposure Reading)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   In February 1840 the Crown presented Maori rangatira with a treaty in two
 *   languages. The English text ceded all the rights and powers of
 *   sovereignty; the Maori text the chiefs actually signed granted
 *   kawanatanga — a newly coined word for governorship — while guaranteeing
 *   tino rangatiratanga, full chieftainship, over lands, villages, and
 *   taonga. This story authors the retrospective_snare_exposure reading: the
 *   divergence between the texts was not an accident complicating an
 *   otherwise fair bargain but the extraction mechanism itself. Chiefs
 *   assented to what the Maori text said; the Crown acted on what the English
 *   text claimed; land purchasing, the Native Land Court's individualisation
 *   of title, the wars and confiscations, and a century of overriding
 *   legislation all proceeded under a translation the signatories could not
 *   have understood and were given no means to contest. The mechanism was
 *   covert at operation — it looked like a treaty — and became legible only
 *   retrospectively, through philology (Ruth Ross, 1972), judicial and
 *   Tribunal inquiry (Lands Case 1987; Te Paparahi o te Raki 2014), and Maori
 *   scholarship. Per the epsilon-invariance principle this is one of three
 *   linked stories over the same kernel, each with its own epsilon: the crown
 *   reading authors a settled-authority constraint with negligible extraction
 *   from its own seat; the retention reading authors a contested-partnership
 *   constraint; this reading authors the extraction structure itself. KEY
 *   AGENTS (by structural relationship): - crown_colonial_administration:
 *   Primary agenda-setter (institutional/arbitrage) — drafted both texts,
 *   chose which governed, directed purchase, war, and statute -
 *   crown_land_purchasing_apparatus: Primary beneficiary
 *   (institutional/arbitrage) — collected the land and the revenue that
 *   funded the colony - colonial_settler_agriculturalists: Secondary
 *   beneficiary (organized/mobile) — received the land, elected the
 *   parliaments - maori_signatories_rangatira: Primary target
 *   (moderate/trapped) — assented to the Maori text only; bore the difference
 *   - maori_descendant_communities: Inherited target
 *   (moderate/identity_locked) — hold the residue and the claim -
 *   kingitanga_confederation: Organized target (organized/trapped) —
 *   halted-loss movement, punished by confiscation - non_signatory_hapu:
 *   Excluded seat (moderate/trapped) — never assented, subjected anyway,
 *   least served by remedies - waitangi_tribunal: Analytical observer
 *   (institutional/analytical) — investigates, finds, recommends; binds no
 *   one
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.58).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.28).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.58).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Textual Divergence as Extraction Mechanism (Retrospective Snare Exposure Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'efa1cd7f-7350-4294-8859-4241f7980237').
narrative_ontology:cs_kernel_codification('efa1cd7f-7350-4294-8859-4241f7980237', fixed_text).
narrative_ontology:cs_authority_grounding('efa1cd7f-7350-4294-8859-4241f7980237', extraction).
narrative_ontology:cs_interpretation_layer_present('efa1cd7f-7350-4294-8859-4241f7980237').
narrative_ontology:cs_reading_relation('efa1cd7f-7350-4294-8859-4241f7980237', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('efa1cd7f-7350-4294-8859-4241f7980237', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('efa1cd7f-7350-4294-8859-4241f7980237', foundational, assent_bounded_by_presented_text).
narrative_ontology:cs_axiom_status(assent_bounded_by_presented_text, holdable).
narrative_ontology:cs_axiom_grounding('efa1cd7f-7350-4294-8859-4241f7980237', assent_bounded_by_presented_text, deontological).
narrative_ontology:cs_axiom('efa1cd7f-7350-4294-8859-4241f7980237', secondary, divergence_enabled_unconsented_transfer).
narrative_ontology:cs_axiom_status(divergence_enabled_unconsented_transfer, holdable).
narrative_ontology:cs_axiom_grounding('efa1cd7f-7350-4294-8859-4241f7980237', divergence_enabled_unconsented_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('efa1cd7f-7350-4294-8859-4241f7980237', maori_text_operative_assent).
narrative_ontology:cs_drift_state('efa1cd7f-7350-4294-8859-4241f7980237', post_northland_tribunal_findings, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('efa1cd7f-7350-4294-8859-4241f7980237', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, colonial_settler_agriculturalists).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories_rangatira).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, kingitanga_confederation).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, non_signatory_hapu).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, english_text_supremacy_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, treaty_domestic_non_justiciability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted both language versions of the founding instrument through its officials and missionary translators, appointed the governors, and thereafter decided which version governed any dispute. Directed the land purchase programme, proclaimed the wars and the confiscations, and passed the statutes that converted communal holdings into individual titles. When challenged, it retreated into the position that its own courts had declared the instrument unenforceable domestically, so nothing owed under it could be sued upon. Exit was easy: it could reframe, delay, or legislate, and no external forum could compel it for over a century.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% The departmental machinery of purchase commissioners, surveyors, interpreters, and land-fund administrators that negotiated deeds, advanced payments, and resold parcels to settlers at multiples of the price paid. Its receipts funded a large share of early colonial government expenditure. It wrote the deed templates, controlled the interpreters, and recorded its own compliance. It needed no exit because it was the paying and receiving counterparty at once.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter).

% Received the land once titles were extinguished or converted, at prices set by the resale market rather than the original payment. Formed the electorate that returned the parliaments which passed the land statutes and voted the military budgets. Could move on to towns or to Australia and carried no obligation backward; the arrangement cost them nothing they had not already been paid in land.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_settler_agriculturalists, beneficiary,
    organized, biographical, mobile, national).

% Several hundred hereditary leaders who gathered at Waitangi and around the country in 1840. They debated at length and many signed the Maori-language sheets placed before them, understanding the governorship grant as a check on settler lawlessness while their own authority over their people and places continued. They had no copy of the English text, no notice that it claimed more, and no forum in which the difference could be raised. Over the following decades their capacity to refuse sales, expel surveyors, or govern their own districts was answered with soldiers, statutes, and courts they did not staff.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories_rangatira, payer,
    moderate, generational, trapped, regional).

% Inherit the outcome: a few percent of the former land base, fragmented titles, lost rivers and taonga, and a grievance history administered through a tribunal and settlement process whose parameters — mandate requirements, fiscal caps, full-and-final clauses — are set by the other party. Their relationship to the land and to the instrument is constitutive of who they are; abandoning the claim is not an available move in the way selling a shareholding is.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities, payer,
    moderate, generational, identity_locked, national).

% The pan-tribal movement formed in the 1850s to halt further land loss by installing a Maori king alongside the governor. Its Waikato heartland was invaded in 1863-64 and roughly 1.2 million acres confiscated irrespective of individual loyalty. It survived as a continuing institution and remains a distinct seat of Maori authority that the settlement process has never fully accommodated.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, kingitanga_confederation, payer,
    organized, generational, trapped, regional).

% Hapu whose leaders never signed any sheet — some because they rejected the governor outright, some because no sheet ever reached them — yet who were subsequently treated as having ceded whatever the English text claimed. They fall outside many settlement frameworks, which presuppose mandate structures descending from signatories, leaving them with the burdens of the arrangement and the least access to its remedies.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, non_signatory_hapu, excluded,
    moderate, generational, trapped, regional).

% A standing commission of inquiry established in 1975, empowered from 1985 to investigate Crown acts and omissions back to 1840 against the principles of the instrument. It hears claimant communities, produces findings — including the 2014 conclusion that the northern rangatira did not cede sovereignty — and recommends remedies the government may accept or decline. It decides nothing binding; its influence runs through reportage and political pressure.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single recognised interface between an expanding settler-colonial state and several hundred autonomous hapu: one address for law and order, one channel through which land transactions could proceed, and a mutual guarantee — Crown preemption of purchase in exchange for protection of rangatiratanga — where previously each rangatira dealt with visiting ships, traders, and speculators separately.
% TRANSFER_FUNCTION: Moved land — ultimately the overwhelming majority of the country's area — from communal Maori holding to Crown title and onward to settler freehold; moved governing authority from hapu and rangatira to a Westminster-style parliament in which Maori held minimal representation for a century; moved purchase payments, resale margins, and land-fund revenue into the Crown's accounts; and moved the costs of policing the resulting dispossession onto those dispossessed.
% ABSENT_VOICES: The rangatira who refused to sign and the hapu never approached sit outside the settlement architecture, which presumes descent from signatory mandates. Generations already dead when the Tribunal opened cannot testify, though their petitions and oral records survive in the archive. Within contemporary negotiations, dissenting voices inside claimant communities — those who reject full-and-final clauses — are structurally muted by the mandate rules the Crown imposes. They are absent from the table because the table's rules were written by the counterparty.
% DISAPPEARANCE_RATIONALE: Nearly all private title in the country traces through Crown grants that trace through the instrument. Overnight removal would cloud the foundation of the entire property system, dissolve the tribunal and settlement machinery mid-stream, reopen the authority question between the Crown and hapu with no agreed procedure, and strand hundreds of settled and unsettled claims alike.
% FOUNDING_PROBLEM: Securing a stable, exclusive imperial foothold before rival powers arrived, and bringing order to a frontier where the New Zealand Company's speculative purchases and settler lawlessness were outrunning any authority — the Colonial Office dispatched Hobson to obtain sovereignty by treaty rather than conquest.
% FOUNDING_PROBLEM_CORROBORATION: No living party defends the founding problem as still operative: the empire that wanted the foothold has dissolved and the country has been sovereign for generations. Historians outside the beneficiary set — Ruth Ross's 1972 philological analysis and Claudia Orange's archival history — attest the founding problem was instrumental and time-bound; the Tribunal's Te Paparahi o te Raki report (2014) attests that what the rangatira agreed to was not what the English text claims. The corroborating consensus is that the arrangement outlived its stated purpose and now functions mainly to administer the consequences of its own making.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.58 at interval end, peaking 0.92 circa 1880) because the value transferred was near-total — from effectively the whole country to a few percent retained — and because the settlement process returns a small fraction of losses under counterparty-set terms. Suppression (0.28 now, 0.83 at peak) tracks the enforcement record: invasion, confiscation, the individualising court, and later police action against protest occupations. Theater (0.38) is moderate with a distinctive shape: the mechanism needed little performance while it worked covertly — apparent genuineness did the work — so theater was lowest during the peak-extraction decades; it grew as open force receded and legitimation had to be performed, dipped when the Tribunal made inquiry functional again, and has risen modestly as commemoration and principles-rhetoric overlay a real but partial settlements process. All three series run on one shared ten-point grid (1840-2026) so no metric is sampled against another's end-state. Claim and metrics are independent: the reading claims snare; the metrics describe what the record shows; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat the instrument is the founding compact of the polity, imperfectly translated but bindingly accepted, with grievances addressed through an unprecedented inquiry process. From the signatory and descendant seats the same document is a substitution preserved in writing: assent given to one text, sovereignty taken under another. From the Kingitanga seat it is a broken guarantee answered by invasion. From the tribunal seat it is a ledger of breaches awaiting remedy. The engine computes these divergences from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The purchase apparatus and settler agriculturalists sit at the beneficiary end: they received land and revenue and bore no cost they could not pass on; the apparatus additionally controlled the deed templates and interpreters, so its derived directionality reflects collection plus administration. The Crown administration sits near them — it captured sovereignty and the land fund — though its seat is agenda-setting rather than simple collection. Signatories, descendants, the Kingitanga, and non-signatory hapu sit at the target end, with exit graded from trapped (signatories facing soldiers; non-signatories outside every remedy framework) to identity_locked (descendants, for whom the claim is constitutive rather than alienable). The tribunal is analytical: it neither pays nor collects, and its recommendations bind no one. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and an override keyed only to power atom would misplace the tribunal along with the Crown seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a snare rather than a degraded rope blocks the standard misreading in which the arrangement began as fair dealing that later went wrong: on this reading the divergence was load-bearing from the first week — the extraction ran through the mismatch itself, so there was never a coordination-only phase to restore. Equally, the omegas block the opposite error of treating every element as fraud: if the translator's rendering was an honest attempt at untranslatable concepts, part of the extraction was added afterward by unilateral interpretation, and the repair target shifts from the text to the interpretive machinery. The founding problem is dead and the arrangement persists — the status-by-verdict mismatch flags the zombie condition — but the persistence is not inertial theater: the machinery actively processes claims and allocates remedies, so the indicated response is restructuring the frame, not retirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which of the three readings of the treaty_authority_cession kernel should govern assessment — and is this reading''s claim that the divergence itself was the mechanism compatible with the sibling frames?',
    'Not resolvable by data alone: the disagreement is located in whether assent is measured by the text presented to the assenting party (this reading) or by the drafting intent of the English text (crown reading), with the retention reading splitting the difference procedurally. Comparative constitutional treatment of bilingual instruments and contra proferentem doctrine narrows but does not close it.',
    'Adopting the crown reading empties this story''s victim set and reclassifies the arrangement toward settled authority; adopting the retention reading converts the mechanism question into a partnership-design question; this reading stands only where assent-bounding holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest among three readings; the disagreement is located in the measure of valid assent.').

omega_variable(
    translator_intent_ambiguity,
    'Was the missionary translator''s rendering a deliberate widening of the gap between the texts, or an honest attempt to express concepts (sovereignty, preemption) with no Maori equivalents?',
    'Philological reconstruction from the translator''s papers, Busby''s 3 February draft versus the 4 February back-translation, and missionary correspondence; comparison with the translator''s own earlier consistent usage of rangatiratanga for possession and authority.',
    'Deliberate divergence supports the full snare reading with the text itself as the trap; honest approximation relocates part of the extraction to post-1840 interpretive practice, lowering epsilon at origin and shifting the repair target from the document to the institutions that exploited it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translator_intent_ambiguity, empirical, 'Whether the textual divergence was engineered or emergent.').

omega_variable(
    retrospective_visibility_status,
    'Was the mechanism a snare from the first signature that merely awaited detection, or a genuinely ambiguous compact that hardened into extraction only through subsequent unilateral practice?',
    'Counterfactual institutional analysis: would contemporaneous good-faith reconciliation of the texts — as the early exchange of correspondence partially attempted — have altered the land-transfer trajectory, or was the divergence already doing the work?',
    'Snare-from-origin preserves the claimed type with epsilon high throughout the interval; hardening-over-time dates the snare character later and renders the early decades a hybrid coordination-extraction structure instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_visibility_status, conceptual, 'Timing of the snare character relative to its retrospective detectability.').

omega_variable(
    settlement_sufficiency,
    'Does the contemporary settlement programme — roughly two billion dollars in aggregate against losses measured in land worth vastly more, under full-and-final and relativity constraints — materially retire the extraction, or extend it under procedural form?',
    'Track post-settlement asset trajectories, relativity-clause escalations (Waikato-Tainui, Ngai Tahu), and unresolved claim classes (foreshore and seabed, freshwater, non-signatory hapu) against the counterfactual of unconstrained restitution.',
    'If sufficient, end-state epsilon falls toward a residual-coordination profile and the snare reads as historically bounded; if insufficient, the standing arrangement is the same mechanism wearing process, and epsilon stays high indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_sufficiency, empirical, 'Whether the redress machinery retires or extends the extraction.').

omega_variable(
    victim_coalition_capacity,
    'Can the victim seats convert dispersed grievance into coalition power — pan-iwi coordination, international indigenous-rights mechanisms, the Matike Mai constitutional conversation — sufficient to change the bargaining structure?',
    'Observe whether collective vehicles (National Iwi Chairs Forum, Matike Mai Aotearoa, UNDRIP implementation reviews) produce binding changes in mandate rules, fiscal envelopes, or the full-and-final architecture.',
    'Effective coalition raises the resistance ceiling above the authored 0.75 and moves trapped and identity_locked seats toward organized and mobile, compressing effective extraction from the target side; failure leaves the current asymmetry intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_capacity, empirical, 'Coalition potential of the victim set against counterparty-set procedures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.12).
narrative_ontology:measurement(trea_tr_t1865, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1865, 0.16).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.26).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.33).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.34).
narrative_ontology:measurement(trea_tr_t1985, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1985, 0.31).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.5).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.78).
narrative_ontology:measurement(trea_be_t1865, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1865, 0.86).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.92).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.87).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.84).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.72).
narrative_ontology:measurement(trea_be_t1985, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.62).
narrative_ontology:measurement(trea_su_t1865, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1865, 0.8).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.83).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.76).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.44).
narrative_ontology:measurement(trea_su_t1985, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1985, 0.37).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% Constraint family over the treaty_authority_cession kernel: crown_cession_reading (upstream, settled-authority claim, negligible epsilon from its own seat), rangatiratanga_retention_reading (contested-partnership claim), and this story (extraction-mechanism claim, high epsilon). The colloquial label 'the Treaty' covers all three; they are separated because measuring assent by the English text, by the Maori text, or by the divergence itself yields different epsilon, different victim sets, and different types. Each member links the others via affects_constraints; upstream members are cited as authority for downstream ones — the crown reading's textual-cession claim is precisely what this reading identifies as the operative fiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
