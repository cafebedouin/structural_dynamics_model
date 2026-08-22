% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Shinbutsu-Shugo as Institutionally Tolerated Incoherence (Incoherence Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   For roughly eleven centuries (c. 741-1868), Japanese religious
 *   institutions operated under shinbutsu-shugo: the fused administration of
 *   kami shrines and Buddhist temples, in which head temples held betto
 *   authority over shrines, monks served as shrine clergy, kami were ritually
 *   assimilated to buddhas, and — under the Tokugawa parishioner system —
 *   every household registered with a Buddhist temple. This story
 *   instantiates ONE reading of the contested kernel
 *   shinbutsu_ontological_commitment: the incoherence_reading, which holds
 *   that no stable ontological commitment ever anchored the arrangement — it
 *   was institutionally tolerated incoherence, kept running by administrative
 *   routine and vested interest rather than by a shared cosmology. The
 *   sibling readings are separate constraint stories linked via
 *   network.affects_constraints: the syncretic_reading holds kami and buddhas
 *   were aspects of one unified honji-suijaku order; the partition_reading
 *   holds the two systems occupied stable, separate functional domains
 *   without ontological integration. Because the readings characterize the
 *   same standing arrangement with different epsilon, they are separate
 *   stories per the epsilon-invariance principle: this reading authors
 *   epsilon ~0.52 for the fused arrangement as the incoherence reading
 *   assesses it (real coordination delivered, real asymmetric capture layered
 *   on top, no deep commitment holding it); the syncretic reading would
 *   author lower epsilon; the partition reading would decompose the referent.
 *   Claim/metric independence: claimed_type tangled_rope states this
 *   reading's structural judgment (genuine coordination function + asymmetric
 *   capture + active enforcement); the metrics are authored independently as
 *   descriptive estimates, and the engine computes per-seat classifications
 *   from the structural data. The Meiji separation edicts of 1868 are the
 *   interval terminus: the arrangement's near-instant collapse under
 *   administrative fiat is this reading's central evidentiary datum, routed
 *   through omegas rather than folded into the metrics.
 *
 * KEY AGENTS:
 *   - great_temple_establishments: agenda-setting administrator (institutional/arbitrage) — holds betto control over shrines, directs revenues and appointments
 *   - buddhist_clergy_administering_shrines: secondary beneficiary (organized/constrained) — staffs the fused sites under temple discipline
 *   - tokugawa_bakufu: co-administrator (institutional/arbitrage) — runs the parishioner-registration machinery that hardens temple-side control
 *   - meiji_state_builders: retrospective beneficiary (institutional/mobile) — inherits the arrangement's separability as state-building raw material
 *   - hereditary_shrine_priest_lineages: primary bearer of costs (moderate/identity_locked) — hereditary office subordinated to temple administration
 *   - local_kami_cult_communities: diffuse cost-bearers with incidental receipts (powerless/constrained)
 *   - nativist_kokugaku_scholars: excluded critic (moderate/constrained) — objects to the fusion, holds no administrative seat
 *   - religious_studies_historians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.52).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.32).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.33).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-Shugo as Institutionally Tolerated Incoherence (Incoherence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '53959017-5077-49da-a17f-e6be5570cec6').
narrative_ontology:cs_kernel_codification('53959017-5077-49da-a17f-e6be5570cec6', distributed).
narrative_ontology:cs_authority_grounding('53959017-5077-49da-a17f-e6be5570cec6', distributed).
narrative_ontology:cs_reading_relation('53959017-5077-49da-a17f-e6be5570cec6', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('53959017-5077-49da-a17f-e6be5570cec6', shinbutsu_ontological_commitment__partition_reading, influences).
narrative_ontology:cs_axiom('53959017-5077-49da-a17f-e6be5570cec6', foundational, no_stable_ontological_kernel_existed).
narrative_ontology:cs_axiom_status(no_stable_ontological_kernel_existed, holdable).
narrative_ontology:cs_axiom_grounding('53959017-5077-49da-a17f-e6be5570cec6', no_stable_ontological_kernel_existed, empirically_contingent).
narrative_ontology:cs_axiom('53959017-5077-49da-a17f-e6be5570cec6', secondary, institutional_interest_substituted_for_doctrine).
narrative_ontology:cs_axiom_status(institutional_interest_substituted_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('53959017-5077-49da-a17f-e6be5570cec6', institutional_interest_substituted_for_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('53959017-5077-49da-a17f-e6be5570cec6', tolerated_doctrinal_plurality).
narrative_ontology:cs_drift_state('53959017-5077-49da-a17f-e6be5570cec6', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('53959017-5077-49da-a17f-e6be5570cec6', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, great_temple_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, buddhist_clergy_administering_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, hereditary_shrine_priest_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, local_kami_cult_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_kami_cult_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great temple complexes (Enryaku-ji, Kofuku-ji, and their networks) hold head-temple authority over major shrines: they appoint the monks who serve there, direct shrine revenues, festivals, and rebuilding, and petition or litigate to defend these rights. Their holdings span the archipelago, and they can shift resources and personnel between the temple and shrine sides of their estates as court or bakufu policy moves.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, great_temple_establishments, agenda_setter,
    institutional, generational, arbitrage, national).

% Monks stationed at shrines under head-temple discipline. They perform Buddhist rites for the kami, keep the shrine's ritual calendar, collect offerings and stipends, and answer upward to the head temple. Leaving the station means leaving rank and livelihood inside the temple hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_clergy_administering_shrines, beneficiary,
    organized, biographical, constrained, regional).

% The warrior government administers the parishioner system: every household registers annually with a Buddhist temple, which certifies it is not Christian, giving the bakufu census, surveillance, and funerary regulation through the temple network. It ratifies temple-shrine appointments and grants tax exemptions to the great complexes, trading fiscal revenue for order and information.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, beneficiary).

% The new imperial government after 1868 wants a national cult centered on the emperor, distinct from the 'foreign' religion. It finds the fused arrangements can be unbundled by administrative fiat: edicts order kami and buddhas separated, shrines are made independent, clergy are reassigned, and State Shinto is built on the cleared ground within a few years.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary,
    institutional, generational, mobile, national).

% Families holding hereditary custodianship of particular shrines across generations. Office, income, and marriage alliance are bound to a shrine that sits under head-temple administration; petitions against temple control rarely succeed, and setting the office aside would mean setting aside the lineage's name, livelihood, and standing.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, hereditary_shrine_priest_lineages, payer,
    moderate, generational, identity_locked, regional).

% Villages and towns whose festivals, processions, and patron-deity cults run through local shrines. They fund festivals and repairs, receive mortuary and protective rites through the fused temple-shrine complex, and have little voice in who administers the shrine above them; occasionally they protest levies through collective unrest.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_kami_cult_communities, payer,
    powerless, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, local_kami_cult_communities, beneficiary).

% Edo-period scholars of native learning who argue the kami cult predates and outranks Buddhism, that fusion doctrines are foreign contamination, and that the ancient classics show an unmixed way of the gods. They publish, teach, and cultivate domain-lord patronage, but hold no administrative seat in the temple-shrine system until its final years.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, nativist_kokugaku_scholars, excluded,
    moderate, generational, constrained, national).

% Modern analysts working from land registers, appointment documents, doctrinal treatises, and edict texts. They reconstruct who controlled which sites, trace revenue flows, and compare the rival doctrinal constructions; they hold no position in the arrangement and can see the whole structure at once.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, great_temple_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Running two cult systems — an autochthonous kami cult tied to locality, lineage, and imperial ritual, and a continental Buddhist soteriology with its own ordination, landholding, and doctrinal apparatus — in one archipelago without zero-sum jurisdictional war: shared sites, shared personnel, shared financing, and mutual legitimation solved the coexistence problem once, centrally, instead of per-cult.
% TRANSFER_FUNCTION: Moved revenue, labor, and ritual authority from local kami cults and their hereditary priestly lineages upward into Buddhist institutional hierarchies (offerings, shrine revenues, and after 1600 parishioner registration and funerary fees), and moved legitimation and doctrinal prestige downward onto the kami cults; after 1868 the same plumbing moved shrine assets and clerical labor to the new state.
% ABSENT_VOICES: Nativist kokugaku scholars objected that the fusion was incoherent foreign contamination and that the classics showed an unmixed way of the gods; they published and petitioned but held no administrative seat until the system's final years. Independent kami-priest reformers petitioned against temple control with little success. Village practitioners' own understandings — often neither doctrinally Buddhist nor 'Shinto' — enter the record only obliquely, through festival regulations and dispute settlements.
% DISAPPEARANCE_RATIONALE: When the separation edicts landed in 1868 the rearrangement was immediate and vast: shrines seceded from temples, Buddhist objects and kami-statue assimilations were stripped from tens of thousands of sites, the betto system and the parishioner machinery were abolished, a wave of anti-Buddhist destruction (haibutsu kishaku) closed thousands of temples, and State Shinto was erected on the cleared ground. Nothing about the surrounding society required the fused arrangement to persist; what rearranged was the religious-administrative map itself.
% FOUNDING_PROBLEM: Integrating an imported universal salvation religion (Buddhism) with an entrenched autochthonous cult (the kami) — managing the collision between continental doctrinal authority and native cultic prerogative so that neither side destroyed the other and both could operate in the same territory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nativist kokugaku scholars (Motoori Norinaga and successors) attested from an adversarial seat that the fusion was doctrinally incoherent overlay rather than living necessity; Meiji administrators acted on the premise that separation was administratively feasible; and modern historiography — notably Kuroda Toshio's reconstruction of the medieval temple-shrine power system — reads the arrangement as elite institutional politics rather than popular ontological consensus. No party outside the arrangement's beneficiaries attests that the founding integration problem remained live in the later centuries.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.52 (matching the terminal measurement) because the arrangement combined real service delivery with real asymmetric capture: shrine revenues, offering streams, and — after 1600 — parishioner registration and funerary fees flowed upward into temple administrations, while hereditary shrine lineages bore subordination without proportional return. Suppression is authored at 0.32 as a CHARACTERISTIC value across the run, deliberately not equal to the terminal series point: the scalar describes the arrangement's typical enforcement intensity (court ratification, appointment control, and at the Edo peak compulsory parishioner registration with anti-Christian surveillance), while the suppression_requirement series separately documents the Edo ratchet (0.15 rising to 0.48) and the terminal abolition (0.12) when the edict switched the enforcement machinery off. Suppression is a raw structural property and is not scaled by directionality or scope; only extractiveness is scaled, by the engine. Theater_ratio rises from 0.18 to 0.48 as the doctrinal superstructure (honji-suijaku treatises, ryobu and Sanno constructions, Yoshida inversions) increasingly performed a coherence that, on this reading, was never load-bearing — legitimation work substituting for settled commitment. Accessibility_collapse 0.42: alternatives never fully closed — rival constructions proliferated inside the arrangement, nativist purism survived at its margins, and hidden Christian practice persisted under repression. Resistance 0.33: shrine-lineage petitions, kokugaku critique, and occasional rural unrest met the arrangement without dislodging it until the state itself repudiated it. All three tracked series share one eight-point grid (741-1868); no metric is sampled on a private grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the great temple establishments' position the arrangement is patrimony: a jurisdictional order they administer, staff, and finance, experienced as coordination they run. From the hereditary shrine lineages' position the same order is subordination: their offices, incomes, and marriages sit inside a hierarchy they did not set and cannot readily leave — and their exit is identity_locked, since the lineage has become its shrine office across generations. The lock is jointly institutional and relational: when the frame broke in 1868, many lineages rebranded overnight as State Shinto priests, which suggests the structural component dissolved with the edict while the identitarian component had always been thinner than it appeared; the village-level commitment omega probes the residual. Village communities experience a mixed receipt: festivals and mortuary services through the fused complex, revenue and deference flowing out of it. Inter-institutionally, the Tokugawa bakufu administered the machinery while bearing its fiscal costs (temple tax exemptions), and the Meiji state — outside the arrangement's operating lifetime — converted its separability into state-building capital. Same-level lateral divergence: temple establishments and shrine lineages occupy the same nominal religious sphere at different effective power, differentiated by appointment control and land title rather than by doctrine. Coalition note: village communities held latent collective power (rural unrest against temple levies recurred) but never coordinated across regions against the arrangement itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: great_temple_establishments (agenda_setter and beneficiary, institutional power, arbitrage exit) derive near the full-beneficiary end; buddhist_clergy_administering_shrines (beneficiary, constrained) sit low; meiji_state_builders (beneficiary via separability, mobile) sit nearest the subsidy end despite acting only at the terminus. Victim declarations drive high d: hereditary_shrine_priest_lineages (payer, identity_locked, generational horizon) derive near the full-target end; local_kami_cult_communities (payer with a secondary beneficiary position, constrained) derive high but not maximal. The bakufu is genuinely dual-positioned — it collects surveillance value and bears exemption costs — but directionality overrides are keyed by power atom, so any correction authored for 'institutional' would misapply to the temple seats and the Meiji seat; no override is authored and the nuance is carried in prose. Kokugaku scholars are excluded rather than coordinated — a commentary-grade absence, not a correction-grade input. Spatial scope is national for the administering seats and local for the paying villages; larger scope makes verification harder and modestly amplifies effective extraction, arithmetic the engine owns.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — absorbing an imported universal soteriology into an entrenched autochthonous cult without either destroying the other — was substantially solved within the first centuries of the arrangement; what persisted afterward was interest and routine, not the original collision. The classification prevents two symmetrical errors: reading the arrangement as snare would erase the genuine coordination (shared sites, personnel, finance, mutual legitimation) that a millennium of practice delivered; reading it as rope would erase the asymmetric capture (revenue flow, subordination of shrine lineages, compulsory registration) that the same structure maintained. Tangled_rope holds both halves. The terminal dynamics distinguish it from piton: the arrangement did not outlive its function as theatrical residue — it was repudiated and dismantled while still functionally alive, and the speed of that dismantling is itself this reading's evidence that little ontological depth held it up. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) flags the post-functional persistence phase honestly: roughly the last four centuries ran on a mandate already discharged, sustained by the receipt structure documented in gain_flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the incoherence_reading of the kernel shinbutsu_ontological_commitment. How would instantiating the syncretic_reading or the partition_reading change the structural data — epsilon, beneficiary/victim sets, and per-seat classification?',
    'Author the sibling stories as separate constraint files over the same standing arrangement and compare engine outputs: epsilon authored by each reading''s own lights, per-seat classifications, and the network coupling among the three.',
    'Under the syncretic_reading, epsilon drops (a genuine unified order renders the transfers coordination cost rather than rent) and the victim set thins; under the partition_reading, the referent decomposes into two weaker domain-specific constraints with reduced coupling. This story''s classification is valid only relative to its reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the shinbutsu ontological kernel governs the classification.').

omega_variable(
    separation_ease_inference,
    'Does the speed and completeness of the 1868 collapse evidence pre-existing shallow integration (this reading''s central inference), or is it an artifact of unprecedented Meiji state capacity plus haibutsu-kishaku opportunism?',
    'Comparative analysis of separation attempts by pre-modern states elsewhere; counterfactual capacity analysis of what the Tokugawa bakufu could have executed; micro-studies of village-level attachment to fused practice at the moment of separation.',
    'If coercion capacity fully explains the collapse, the incoherence reading loses its strongest warrant; epsilon and the theater trajectory would be re-read toward the partition_reading''s stability claims, and the arrangement''s persistence would look better grounded than this story allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_ease_inference, empirical, 'Whether separation ease measures shallow integration or state capacity.').

omega_variable(
    meiji_state_seat_status,
    'Is the Meiji state a beneficiary OF the standing arrangement (subsidized by its separability) or merely the agent of its destruction — should it sit in the beneficiary set feeding directionality?',
    'Conceptual analysis of what benefiting-from-a-constraint''s-structural-property commits the derivation to, plus a sensitivity run of the engine with the seat excluded from the beneficiary declarations.',
    'Excluding the seat concentrates the arrangement''s computed asymmetry on the temple-side beneficiaries and slightly raises effective extraction for the remaining structure; keeping it spreads subsidy credit to an actor outside the arrangement''s operating lifetime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_state_seat_status, conceptual, 'Whether the Meiji state belongs in the beneficiary structure of the pre-1868 arrangement.').

omega_variable(
    persistence_mechanism,
    'What held the arrangement up for eleven centuries if no stable ontological commitment did — institutional interest, administrative path dependence, or the simple absence of any demand for coherence?',
    'Process-tracing of maintenance decisions (appointments, boundary disputes, rebuilds): do actors defend the fusion itself, or merely their positions within it? Count how often doctrinal consistency is invoked versus jurisdictional advantage in the documentary record.',
    'Interest-driven persistence supports the tangled_rope reading; pure inertia would push late-period dynamics toward piton-like decay; demonstrated absence of demand for coherence corroborates the incoherence thesis directly and lowers the weight of the enforcement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism, empirical, 'The mechanism sustaining a commitment-less arrangement across centuries.').

omega_variable(
    village_level_commitment_depth,
    'How deep did ontological commitment run at the village level, beneath elite doctrinal production — did practitioners hold kami-buddha unity as real, or as irrelevant background to festival and mortuary practice?',
    'Local histories, festival regulations, mortuary-practice studies, and legend/oracle corpora from the Edo period; attention to what villagers actually contested when disputes arose (levies and precedence, almost never doctrine).',
    'Deep popular commitment would lower epsilon and partially vindicate the syncretic_reading''s sociological half even under this reading''s institutional claims; shallow commitment strengthens the separation-ease evidence and this story''s suppression profile as purely administrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(village_level_commitment_depth, empirical, 'Depth of lay commitment beneath the elite doctrinal surface.').

omega_variable(
    cs_framing_underdetermination,
    'Is the arrangement''s authority best modeled as distributed (rival schools — Tendai, Shingon, Yoshida, Sanno — producing competing constructions with no designated interpreter) or as practice-grounded with a functioning interpretive layer (the schools collectively absorbing drift for centuries)?',
    'Test both framings against the commitment-system pattern classes: whether any school ever won adjudication rights over the kernel, and whether the schools'' output functioned as interpretation-of-a-kernel or as independent kernel-production.',
    'Under practice-grounding with an interpretation layer, the commitment-system classification shifts toward a stabilized-kernel pattern, which would sit uneasily with this reading''s instability claim; under distributed coding (adopted here), the unstable-kernel pattern matches the incoherence thesis. The choice is consequential and not forced by the data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative defensible framings of the arrangement''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 741, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t741, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 741, 0.18).
narrative_ontology:measurement(shin_tr_t950, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 950, 0.24).
narrative_ontology:measurement(shin_tr_t1150, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1150, 0.3).
narrative_ontology:measurement(shin_tr_t1350, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1350, 0.33).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1550, 0.36).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1700, 0.42).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1800, 0.46).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.48).

% Extraction over time
narrative_ontology:measurement(shin_be_t741, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 741, 0.34).
narrative_ontology:measurement(shin_be_t950, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 950, 0.4).
narrative_ontology:measurement(shin_be_t1150, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1150, 0.46).
narrative_ontology:measurement(shin_be_t1350, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1350, 0.51).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1550, 0.54).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1700, 0.58).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1800, 0.56).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t741, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 741, 0.15).
narrative_ontology:measurement(shin_su_t950, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 950, 0.2).
narrative_ontology:measurement(shin_su_t1150, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1150, 0.26).
narrative_ontology:measurement(shin_su_t1350, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1350, 0.3).
narrative_ontology:measurement(shin_su_t1550, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1550, 0.34).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1700, 0.48).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1800, 0.47).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' covers three structurally distinct claims about what bound kami cult and Buddhism together, decomposed per the epsilon-invariance principle into a three-story constraint family: this story (incoherence_reading — no stable commitment; tolerated incoherence; epsilon ~0.52 over the fused arrangement), the syncretic_reading (a unified honji-suijaku cosmological order; would author lower epsilon, reading the transfers as coordination cost), and the partition_reading (stable functional domain-division without ontological integration; would decompose the referent into two domain constraints). Each story carries its own epsilon, beneficiaries, and stakeholders; the upstream/downstream pressure runs from whichever reading prevails toward the others' operating environment. This file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
