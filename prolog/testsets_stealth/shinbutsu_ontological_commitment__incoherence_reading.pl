% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Incoherence
 *   domain: religious/historical
 *
 * SUMMARY:
 *   For roughly eleven centuries kami worship and Buddhism in Japan were
 *   administered as one combined apparatus: shrine-temple complexes
 *   (jingu-ji), Buddhist betto administrators installed over shrines, kami
 *   enshrined as dharma guardians and local traces of original buddhas under
 *   honji-suijaku theorizing, and — from the seventeenth century — compulsory
 *   Buddhist parish registration binding every household to the temple
 *   apparatus. This story instantiates the incoherence_reading of the kernel
 *   shinbutsu_ontological_commitment: the claim that no stable ontological
 *   commitment underlay the arrangement, which persisted as institutionally
 *   tolerated incoherence — sustained by what it delivered to the great
 *   monastic establishments, the court patron families, and finally the
 *   bakufu, and held in place by enforcement rather than by any doctrine its
 *   holders would have defended at cost. The epsilon authored here is for the
 *   standing syncretic arrangement as this reading sees it: moderate-high
 *   (0.58), because the doctrinal superstructure is read as cover over an
 *   upward flow of revenue and authority that the subordinated shrine
 *   lineages never agreed to and could not contest. The sibling stories
 *   author different epsilon over the same referent: syncretic_reading sees a
 *   coherent, valued unified order whose costs are coordination costs (low
 *   epsilon); partition_reading sees administrative scaffolding over two
 *   functionally separate cults (moderate epsilon). Claim and metrics are
 *   independent authored facts: the claimed type is this reading's structural
 *   verdict, and the metrics describe the arrangement's operation as the
 *   record shows it. The expected structural delta — kernel instability,
 *   separation ease, Meiji state-building benefit, syncretic institutional
 *   collapse — is the signature the temporal series is built to display:
 *   accumulation to 1868, then enforced dissolution.
 *
 * KEY AGENTS:
 *   - great_buddhist_establishments: Primary beneficiary and agenda-setter (institutional/arbitrage) — administered shrines through the betto system, collected revenue shares, produced and reframed the doctrinal justifications
 *   - hereditary_shrine_priests: Primary target (moderate/constrained) — performed the kami rites under subordinated standing while revenue and doctrinal authority flowed to the temples
 *   - local_shrine_communities: Payer with incidental beneficiary position (organized/constrained) — funded and labored for the combined order, received its integrated ritual calendar
 *   - court_aristocratic_patrons: Secondary beneficiary (institutional/arbitrage) — charter confirmations, dispute fees, patronage prestige
 *   - tokugawa_bakufu: Late beneficiary (institutional/arbitrage) — compulsory temple registration made the Buddhist apparatus an instrument of governance
 *   - meiji_separation_officials: Terminal agenda-setter (institutional/arbitrage) — administered the arrangement's dismantling and built State Shinto on the cleared ground
 *   - kokugaku_scholars: Excluded voice (moderate/constrained) — nativist critique from outside the arrangement's governance, converted into state policy in 1868
 *   - religious_studies_historians: Analytical observer (analytical/analytical) — reconstructs the structure from charters, revenue records, doctrine, and the separation's cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.55).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/historical").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '58a57774-bcb3-4efe-aba3-d6e58bf18e29').
narrative_ontology:cs_kernel_codification('58a57774-bcb3-4efe-aba3-d6e58bf18e29', distributed).
narrative_ontology:cs_authority_grounding('58a57774-bcb3-4efe-aba3-d6e58bf18e29', practice).
narrative_ontology:cs_interpretation_layer_present('58a57774-bcb3-4efe-aba3-d6e58bf18e29').
narrative_ontology:cs_reading_relation('58a57774-bcb3-4efe-aba3-d6e58bf18e29', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('58a57774-bcb3-4efe-aba3-d6e58bf18e29', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('58a57774-bcb3-4efe-aba3-d6e58bf18e29', foundational, no_stable_ontological_commitment_existed).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment_existed, holdable).
narrative_ontology:cs_axiom_grounding('58a57774-bcb3-4efe-aba3-d6e58bf18e29', no_stable_ontological_commitment_existed, empirically_contingent).
narrative_ontology:cs_axiom('58a57774-bcb3-4efe-aba3-d6e58bf18e29', secondary, institutional_benefit_sustained_fusion).
narrative_ontology:cs_axiom_status(institutional_benefit_sustained_fusion, holdable).
narrative_ontology:cs_axiom_grounding('58a57774-bcb3-4efe-aba3-d6e58bf18e29', institutional_benefit_sustained_fusion, empirically_contingent).
narrative_ontology:cs_reference_frame('58a57774-bcb3-4efe-aba3-d6e58bf18e29', uncommitted_institutional_coexistence).
narrative_ontology:cs_drift_state('58a57774-bcb3-4efe-aba3-d6e58bf18e29', meiji_separation_revelation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('58a57774-bcb3-4efe-aba3-d6e58bf18e29', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, great_buddhist_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, court_aristocratic_patrons).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, hereditary_shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, honji_suijaku_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The great monastic centers and their branch networks (Todaiji, Miidera, Toji and their lineages) supplied the betto administrators installed over major shrines, performed Buddhist rites at shrine sites, and collected shares of shrine land income and offerings. They produced the doctrinal justifications — kami as guardians of the dharma, kami as local traces of original buddhas — and could reframe doctrine when institutional position required, as Yoshida Kanetomo's fifteenth-century inversion of honji-suijaku showed the framing could be turned around without disturbing the underlying arrangement. Leaving meant abandoning shrine revenues and administrative prerogatives accumulated over centuries; adjusting the doctrine to fit was always cheaper.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, great_buddhist_establishments, agenda_setter,
    institutional, generational, arbitrage, national).

% Court noble families held and confirmed the charters that established shrine-temple combinations, took fees for confirmations and dispute settlements, and gained prestige as patrons of the great combined cults. Their income and standing depended on the arrangement's continuation; none of their business required settling what the kami 'really were' relative to the buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, court_aristocratic_patrons, beneficiary,
    institutional, generational, arbitrage, national).

% From the seventeenth century the bakufu required every household to register with a Buddhist temple (the terauke and danka system), tying the temple apparatus into population control and the ban on Christianity. It obtained administrative reliability across the whole country without funding the apparatus, and its stake in the combined religious order was instrumental rather than doctrinal.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, beneficiary,
    institutional, generational, arbitrage, national).

% Priest lineages bound to particular shrines lost administrative control when Buddhist betto were installed above them; they performed the kami rites while revenue shares and doctrinal authority flowed to the administering temples. Their office was tied to a specific sacred site and an inherited lineage, so leaving meant abandoning the vocation and its standing altogether. Major-shrine priest houses negotiated favorable terms; most local lines had no comparable leverage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, hereditary_shrine_priests, payer,
    moderate, generational, constrained, local).

% Village worship communities funded shrine upkeep, provided labor, and bore the revenue shares routed upward to the administering temples; in return they received an integrated ritual year — kami festivals alongside Buddhist funerary and memorial rites, and access to Buddhist teaching about salvation — under one institutional roof. Their religious life was territorial: leaving the arrangement meant leaving the village's entire ritual order, not shopping for another one.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_communities, payer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_communities, beneficiary).

% The new Meiji government's Council of Shinto Affairs and its agents issued the 1868 separation edicts: shrines were purged of Buddhist objects and rites, betto and shrine monks were expelled, shrine-temple complexes were physically divided, and kami worship was reorganized under a state shrine hierarchy. They administered the arrangement's final years — its dismantling — and built State Shinto on the cleared ground. Until 1868 they had held no seat in the arrangement's governance at any point in its thousand-year history.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_separation_officials, agenda_setter,
    institutional, generational, arbitrage, national).

% Nativist scholars argued across the eighteenth and nineteenth centuries that kami worship was an independent tradition overlaid by foreign Buddhist metaphysics, and that the syncretic theology was a late justification rather than the arrangement's ground. They held no seat in the arrangement's governance; their critique circulated through domain schools and academies until the Meiji transition converted it into state policy almost overnight.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, kokugaku_scholars, excluded,
    moderate, generational, constrained, national).

% Modern historians of Japanese religion reconstruct the arrangement from charters, land and revenue records, doctrinal texts, and the cost structure of the separation transition. Kuroda Toshio's re-reading of the medieval Buddhist order and subsequent work on the discursive construction of 'Shinto' frame the standing question this story is one answer to: whether the arrangement embodied a stable ontological commitment or an institutionally maintained habit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, great_buddhist_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solved real integration problems: it gave local kami cults access to Buddhist ritual technology, texts, funding, and teaching about salvation; it operated shared sacred sites under a single administrative hierarchy (the betto system); it integrated village ritual calendars — kami festivals and Buddhist funerary and memorial rites — into one provision system; and it anchored the imported Buddhist institution to established local cults, solving Buddhism's rootedness problem at the price of the kami cults' subordinate standing.
% TRANSFER_FUNCTION: Moved revenue (shrine land income, offering shares, charter-confirmation fees), labor, and doctrinal and administrative authority upward from local shrine communities and shrine priest lineages to the great monastic establishments and court patron families; moved ritual services, legitimacy, and soteriological access downward to the kami cults and their communities. In the Tokugawa phase it additionally moved household registration compliance from the population to the bakufu through the temple apparatus.
% ABSENT_VOICES: Independent kami-worship advocates had no seat for most of the interval: shrine priest lines subordinated under betto administration had no forum to contest the arrangement's terms, and the kokugaku scholars argued from outside for generations before their critique became state policy in 1868. Ordinary ujiko were heard only through village ritual office, never in doctrinal or administrative decisions about the arrangement that ordered their religious lives.
% DISAPPEARANCE_RATIONALE: The arrangement's actual disappearance (1868-1871) rearranged the religious world: shrine-temple complexes were physically split, monastic networks lost shrine revenues and administrative prerogatives, kami worship was reorganized under a state shrine hierarchy, village ritual years were forcibly divided, and State Shinto was built on the cleared ground. Many arrangements demonstrably depended on it — which is why this reading's key evidence is how cheap that rearrangement proved once enforcement flipped: nothing in the arrangement's own logic held it together.
% FOUNDING_PROBLEM: The arrival of Buddhism in the sixth and seventh centuries posed an accommodation problem: how to house an imported soteriological institution alongside established kami cults — giving the kami doctrinal standing within or alongside the Buddhist cosmos, and giving Buddhism local rootedness. Shinbutsu-shugo was the apparatus built to manage that accommodation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by the kokugaku scholars, who held the syncretic theology to be an overlay on an independent kami tradition rather than the arrangement's ground, and by modern historiography (Kuroda Toshio's re-reading of the medieval Buddhist order; subsequent work on the constructed character of 'Shinto'), which attests the accommodation was managed administratively rather than resolved doctrinally. The benefiting parties — the monastic establishments and court patrons — attested the integration as theologically necessary, which is precisely the cover this reading is about. The strongest external corroboration on the status question is material: a founding problem supposedly requiring a millennium of doctrinal maintenance was dissolved within roughly three years at low cost once enforcement flipped, and the doctrinal systems had never converged across a thousand years of churn.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: services genuinely flowed both ways — village communities received Buddhist funerals, memorial rites, and soteriological access; kami cults received funding, texts, and ritual technology — but the standing flow of revenue, land income, and doctrinal authority ran upward to the monastic establishments and court patrons, and the subordination of shrine priest lines was structural rather than incidental. Suppression 0.55: the arrangement was held by charters, monastic institutional power, and finally bakufu compulsion (terauke registration tied to the Christianity ban); independent kami lines were marginalized rather than destroyed and ordinary acquiescence was broad, so suppression is moderate. Suppression is authored as the raw structural property of the arrangement — the enforcement-backed marginalization and compulsion — and is not scaled by scope or directionality; only extractiveness is scaled downstream. Theater_ratio 0.42: the doctrinal layer generated competing, non-converging metaphysical systems (Ryobu, Sanno, and Yoshida Kanetomo's fifteenth-century inversion of honji-suijaku into reverse honji-suijaku) — a doctrine that can be flipped without collapsing the arrangement is, on this reading, not load-bearing, and a substantial share of doctrinal activity was the performance of justification over a structure held by interest and enforcement. Accessibility_collapse 0.45: alternatives (independent kami worship, separate administration) persisted at the margins for the whole interval and were instantiated rapidly once enforcement flipped — the cheapness of the Meiji separation is the direct measure of how incompletely alternatives had collapsed. Resistance 0.40: persistent priestly complaints against betto exactions, jurisdictional disputes, kokugaku critique accumulating over the eighteenth century, and domain-level anti-Buddhist sentiment before 1868 — real but never mass mobilization under the standing enforcement. The temporal series runs on one shared grid (800-1871, eight points, all three metrics at every point): base_extractiveness and suppression_requirement accumulate through the Tokugawa enforcement peak, then fall sharply at 1871 — the terminal fall models enforced dissolution (the state demolished the arrangement's machinery), not reform. The base scalar 0.58 represents the standing arrangement at mature operation; the suppression series falling to near zero exactly as the arrangement disappears is this reading's central evidence that enforcement, not commitment, was load-bearing.
 *
 * PERSPECTIVAL GAP:
 *   From the great_buddhist_establishments seat the arrangement computes as an order it built, staffed, and doctrinally justified — coordination it maintained at real cost. From the hereditary_shrine_priests seat the same structure computes as expropriation with metaphysical cover: their rites performed, their revenues shared upward, their administrative authority held by installed outsiders. The ujiko seat is genuinely dual: burden and provision in the same structure. The sharpest divergence is the meiji_separation_officials seat: they experienced the arrangement purely as an obstacle to state-building, and the structural fact they revealed — that it could be cleared in about three years — is evidence available only from a seat outside the arrangement's own logic. The engine computes these per-seat classifications from the structural data; the divergence between the beneficiary seats' coordination experience and the payer seats' extraction experience is the perspectival gap this story exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (great_buddhist_establishments, court_aristocratic_patrons, tokugawa_bakufu) drive low directionality for those seats; the victim declarations (hereditary_shrine_priests, local_shrine_communities) drive high directionality, with the ujiko's secondary beneficiary position moderating their value below the priests'. The meiji_separation_officials seat is structurally anomalous: they are neither beneficiary nor victim of the arrangement's operation — they bore it as an obstacle during its terminal phase and profited from its absence. Roles are static in this schema while that seat's relationship is time-indexed to the interval's end, so the commentary carries what the structural derivation cannot: their d, assessed against the standing arrangement, sits high (the arrangement taxed their state-building project), but no directionality override is authored because the override surface is power-atom-granular and would misdescribe the other institutional seats sharing that atom. The tokugawa_bakufu's beneficiary position is late-arriving (post-1600); the derived low d is correct for their seat but describes only the interval's final third.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite misreadings. Reading the arrangement as pure coordination would convert the betto revenue system and the subordination of shrine priest lines into 'coordination cost' — erasing the identifiable payers. Reading it as pure extraction would erase the genuine integration the ujiko received and the real coordination problems the apparatus solved. The hybrid holds both: the same charter-and-betto structure that integrated the cults also moved revenue and status upward. The mandatrophy question — did the arrangement outlive its function? — is answered inside the kernel contest: under this reading the founding function (ontological integration) was never genuinely performed, because there was no stable commitment to perform; what persisted past any doctrinal content was the apparatus collecting from its own persistence. The Meiji collapse functions as the mandatrophy test run in the negative: when enforcement flipped and the beneficiaries' claim was withdrawn, nothing in the arrangement's own logic held it up. founding_problem_status is authored contested precisely because whether there was ever a live function to outlive is what the sibling readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story is one reading — the incoherence_reading — of the kernel shinbutsu_ontological_commitment: it holds that no stable ontological commitment underlay shinbutsu-shugo and that the arrangement persisted as institutionally tolerated incoherence. Do the sibling readings characterize the arrangement''s ontological structure better: syncretic_reading (kami and buddhas as aspects of one unified cosmological order under honji-suijaku — a stable, embodied commitment) or partition_reading (stable functional separation of life-cycle and afterlife domains without ontological integration)? The expected structural delta for this reading is kernel instability, separation ease, Meiji state-building benefit, and syncretic institutional collapse; sibling stories would carry different deltas.',
    'Historiographical adjudication: convergence or non-convergence of the doctrinal systems across the interval, whether doctrinal innovation tracks institutional benefit (does the metaphysics move when the institutions'' position moves?), and the cost structure of the Meiji separation as a natural experiment on how much commitment was load-bearing.',
    'If syncretic_reading is correct, this story''s extractiveness is overstated — the arrangement was a coherent order whose costs were coordination costs, and the Meiji collapse was state destruction of a live commitment, shifting the attribution from kernel absence to suppression. If partition_reading is correct, extractiveness is overstated differently — the arrangement was administrative scaffolding over two functionally separate cults, and ''tolerated incoherence'' mischaracterizes a stable division of labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading characterizes the arrangement''s ontological ground.').

omega_variable(
    separation_cost_attribution,
    'Was the Meiji separation cheap because the ontological kernel was never load-bearing (this reading''s claim), or because state coercion was overwhelming enough to crush a live commitment?',
    'Comparative analysis across domains and villages: whether combined worship persisted where separation enforcement was lax (folk-level combined practice in fact persisted in places, and some regions saw violent anti-Buddhist destruction where it did not), plus twentieth-century re-fusion of kami and buddha worship at the popular level.',
    'If local persistence tracked enforcement gaps, the commitment was live and the operative variable is suppression rather than kernel absence — this reading''s central evidence inverts. If separation was cheap wherever enforcement was credible, kernel instability is confirmed and the arrangement sits at the enforcement-held end of its type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_cost_attribution, empirical, 'Whether separation ease evidences kernel absence or overwhelming coercion.').

omega_variable(
    rent_vs_demand_persistence,
    'How much of the arrangement''s persistence is explained by monastic revenue collection (betto shares, shrine land income) versus genuine religious demand for combined worship?',
    'Land registers, revenue-share records, and dispute archives: do institutional efforts track revenue protection or ritual provision? Cross-check sites where revenue flowed out without corresponding ritual services returned.',
    'Revenue-dominance pushes the arrangement toward the pure-extraction end of its type; demand-dominance supports the coordination half and stabilizes the hybrid reading. Also decides whether the ujiko communities'' beneficiary position is substantive or incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_vs_demand_persistence, empirical, 'Revenue collection versus religious demand as the persistence driver.').

omega_variable(
    commitment_level_ambiguity,
    'Does ''ontological commitment'' mean explicit elite doctrinal assent or the practical orientation of ordinary practitioners? Elite texts show competing theorizations (Ryobu, Sanno, Yoshida''s inversion) without convergence; popular practice shows kami and buddhas treated as distinct but cooperating powers within a stable ritual division of labor.',
    'Level-separated analysis: the doctrinal corpus (elite) against practice records (village ritual calendars, popular iconography, festival practice). Elite instability with practical stability is closer to partition_reading; instability at both levels supports this reading.',
    'If commitment assessed at the practical level proves stable, this reading''s ''no stable commitment'' claim fails at the level that matters for persistence and partition_reading absorbs the phenomenon. If both levels show instability, the reading is confirmed and the sibling''s domain-partition order was itself part of the tolerated incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_level_ambiguity, conceptual, 'The level at which commitment is assessed changes the verdict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 710, 1871).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1400, 0.45).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1750, 0.42).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.48).
narrative_ontology:measurement(shin_tr_t1871, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1871, 0.52).

% Extraction over time
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 800, 0.32).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1000, 0.44).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1400, 0.57).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.59).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.65).
narrative_ontology:measurement(shin_be_t1871, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1871, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 800, 0.2).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1200, 0.42).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1400, 0.44).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.58).
narrative_ontology:measurement(shin_su_t1871, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1871, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' covers three structurally distinct claims about the arrangement's ontological ground; per the epsilon-invariance principle they are authored as three stories of one kernel (shinbutsu_ontological_commitment): this incoherence_reading (no stable commitment; epsilon 0.58), syncretic_reading (unified honji-suijaku order; low epsilon as coordination cost), and partition_reading (stable functional domain separation; moderate epsilon). This story links both siblings; meiji_shinbutsu_bunri is the downstream separation policy whose cheapness is this reading's key evidence — the upstream/downstream structure runs from the arrangement's kernel instability (this story) to the separation policy's low cost (meiji_shinbutsu_bunri).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
