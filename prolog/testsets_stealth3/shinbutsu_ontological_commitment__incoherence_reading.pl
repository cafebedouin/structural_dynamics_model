% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Pre-Meiji Kami-Buddha Fusion as Institutionally Tolerated Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the incoherence_reading of the
 *   shinbutsu_ontological_commitment kernel: the claim that pre-Meiji
 *   shinbutsu-shugo — the fused arrangement of kami shrines and Buddhist
 *   institutions — rested on no stable ontological commitment, but was an
 *   institutionally tolerated incoherence sustained by mutual convenience,
 *   temple administration, and bakufu enforcement. The standing arrangement
 *   under assessment (the epsilon referent) is the Tokugawa-era fused
 *   arrangement itself: temple-shrine complexes, compulsory temple
 *   registration of households, and the subordination of kami rites to
 *   Buddhist institutional frames. On this reading the arrangement had a real
 *   but shallow coordination function (a millennium of coexistence without
 *   sectarian war), carried substantial extraction (shrine revenues routed
 *   through overseeing temples, compulsory funerary fees from every
 *   household, and the suppression of the ontological clarification that
 *   would have let kami institutions defend themselves), and collapsed with
 *   striking ease in 1868-1870 — the collapse being the reading's central
 *   evidence that the kernel was never deeply held. CONSTRAINT FAMILY: the
 *   label 'shinbutsu-shugo' decomposes, per epsilon-invariance, into three
 *   readings with different epsilon and different victim structures —
 *   syncretic_reading (unified honji-suijaku order; low extraction),
 *   partition_reading (separate domains; near-zero extraction), and this file
 *   (tolerated incoherence; moderate-high extraction). This story authors
 *   only its own reading; the siblings are separate constraints linked via
 *   network.affects_constraints. The claimed type (tangled_rope) and the
 *   metrics are authored independently: the claim states what this reading
 *   takes to be structurally true; the metrics state what it takes to be
 *   descriptively true of the arrangement's mature phase (c. 1700-1850).
 *
 * KEY AGENTS:
 *   - buddhist_temple_establishment: primary beneficiary and co-administrator (institutional/arbitrage) — ran the jinguji system, collected registration and funerary fees, supplied the doctrinal apparatus
 *   - tokugawa_bakufu: agenda-setter and enforcement backstop (institutional/arbitrage) — mandated temple registration as governance infrastructure and collected the census and surveillance value
 *   - shrine_priestly_lineages: dual-positioned payer/beneficiary (moderate/constrained) — held Buddhist ranks inside the fused economy while seeing kami rites subordinated
 *   - compulsory_registrant_households: primary payer (powerless/trapped) — every household bound to a temple parish, paying for rites it could not refuse
 *   - purist_kami_movements: payer (organized/constrained) — exclusivists and kokugaku scholars suppressed for denying the fusion
 *   - meiji_reform_oligarchs: terminal beneficiary (institutional/arbitrage) — inherited the arrangement's shallowness and converted it into cheap separation
 *   - religious_historians: analytical observer — the post-Kuroda revisionist seat from which this story is authored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.6).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Pre-Meiji Kami-Buddha Fusion as Institutionally Tolerated Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, 'd4902b21-3f30-405e-9d13-3796446659d7').
narrative_ontology:cs_kernel_codification('d4902b21-3f30-405e-9d13-3796446659d7', distributed).
narrative_ontology:cs_authority_grounding('d4902b21-3f30-405e-9d13-3796446659d7', distributed).
narrative_ontology:cs_reading_relation('d4902b21-3f30-405e-9d13-3796446659d7', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('d4902b21-3f30-405e-9d13-3796446659d7', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('d4902b21-3f30-405e-9d13-3796446659d7', foundational, no_binding_ontological_kernel).
narrative_ontology:cs_axiom_status(no_binding_ontological_kernel, holdable).
narrative_ontology:cs_axiom_grounding('d4902b21-3f30-405e-9d13-3796446659d7', no_binding_ontological_kernel, empirically_contingent).
narrative_ontology:cs_axiom('d4902b21-3f30-405e-9d13-3796446659d7', secondary, separation_ease_evidences_kernel_instability).
narrative_ontology:cs_axiom_status(separation_ease_evidences_kernel_instability, holdable).
narrative_ontology:cs_axiom_grounding('d4902b21-3f30-405e-9d13-3796446659d7', separation_ease_evidences_kernel_instability, empirically_contingent).
narrative_ontology:cs_reference_frame('d4902b21-3f30-405e-9d13-3796446659d7', institutional_incoherence_equilibrium).
narrative_ontology:cs_drift_state('d4902b21-3f30-405e-9d13-3796446659d7', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d4902b21-3f30-405e-9d13-3796446659d7', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_reform_oligarchs).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, shrine_priestly_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, compulsory_registrant_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, purist_kami_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shrine_priestly_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, compulsory_registrant_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, coexistence_without_doctrinal_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ruled from Edo and made the Buddhist temple system an instrument of governance: every household was required to register with a temple, giving the bakufu a population census, a check on Christianity, and a lever on local society. It issued the registration ordinances, licensed sectarian hierarchies, and decided which religious movements could preach. It collected governance value from the arrangement while bearing little of its cost; when its own rule collapsed in 1868, the enforcement frame collapsed with it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_bakufu, beneficiary).

% Operated the temple-shrine complexes: temples attached to shrines administered shrine finances, performed Buddhist rites for the kami, and collected funeral and memorial fees from the registered households that sustained them. Sect networks supplied the doctrinal apparatus that framed kami as local manifestations of buddhas. When the Meiji state turned on the arrangement, major temples survived by rebranding as independent sects and shedding their shrine ties.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_establishment, agenda_setter).

% Hereditary shrine priests, many holding Buddhist ranks and names, performed rites under temple oversight and drew income through the fused economy. Some lines prospered inside the arrangement; others watched shrine revenues route through the overseeing temple and kami rites reformatted into Buddhist frames. Priests who petitioned to purify shrines of Buddhist elements were denied for generations — then found their petitions granted within months of 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shrine_priestly_lineages, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, shrine_priestly_lineages, beneficiary).

% Every household had to belong to a Buddhist temple as a registered parish, whatever its actual beliefs. Temples performed funerals and memorial services and charged for them; households paid, hosted priests, and could not transfer affiliation without legal jeopardy. They also shared in the combined festival life of shrine and temple that the fused arrangement made possible — participation they did not choose and could not decline.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, compulsory_registrant_households, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, compulsory_registrant_households, beneficiary).

% Shinto exclusivists — the Yoshida house's anti-Buddhist polemicists, kokugaku scholars such as Motoori Norinaga, and Hirata Atsutane's network of followers — argued that kami worship should be freed from Buddhist framing altogether. They faced censorship, preaching bans, and house arrest; their academies operated under surveillance. Their petitions and polemics supplied the template the Meiji state later enacted from above.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, purist_kami_movements, payer,
    organized, generational, constrained, national).

% The Satsuma-Choshu leadership that seized power in 1868 inherited an arrangement whose institutions had no deep doctrinal loyalty binding them together. They issued the separation edicts, ordered shrines to expel Buddhist objects and priests, and built a state kami cult on the cleared ground. Because few institutions defended the fusion as a matter of conviction, the dismantling took years, not generations — and the cheapness of the dismantling was the arrangement's final gift to them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_reform_oligarchs, beneficiary,
    institutional, generational, arbitrage, national).

% The modern scholarly seat — the post-Kuroda Toshio historiography and the Teeuwen-Rambelli revisionist line — reads temple-shrine records, doctrinal texts, and dispute documents to ask whether the pre-modern fusion rested on a stable ontological commitment. This story is authored from that seat, and its metrics record what that seat takes to be descriptively true of the arrangement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the coexistence problem between an imported, institutionally sophisticated Buddhist complex and indigenous, non-doctrinal kami cults: shared sacred spaces in shrine-temple complexes, pooled endowments and ritual economies, mutual legitimation (kami as local manifestations of buddhas; buddhas as protectors of shrines), and a common festival calendar — sustaining roughly a millennium of coexistence without sectarian war over the kami-buddha boundary.
% TRANSFER_FUNCTION: Moved revenue and ritual authority from kami-cult institutions and registered households to the Buddhist temple establishment and, through it, the bakufu's governance apparatus: shrine finances administered through overseeing temples, compulsory funeral and memorial fees from every registered household, and kami rites framed in Buddhist doctrinal terms.
% ABSENT_VOICES: Purist kami exclusivists spoke but were censored rather than seated; ordinary worshippers had no seat in defining the arrangement at all. Most structurally absent is the ontological question itself — 'what is the relationship between kami and buddhas?' was never institutionally posed, because the arrangement's tolerance depended on no one forcing it. The Meiji oligarchs stood entirely outside the arrangement's conversation before 1868; when they finally entered it, they entered as its executioners.
% DISAPPEARANCE_RATIONALE: It did rearrange, and the rearrangement is the reading's central evidence: when the 1868 separation edicts withdrew and reversed enforcement, shrine-temple complexes were forcibly divided, thousands of temples were destroyed or laicized in the haibutsu kishaku, priests were secularized, and a state kami cult was constructed on the cleared ground within a few years. An arrangement held together by conviction does not collapse this fast or this completely.
% FOUNDING_PROBLEM: The Heian-to-medieval integration problem: Buddhism arrived with a total institutional package — state patronage, doctrinal sophistication, literacy, funeral technology — while kami cults were local, non-doctrinal, and politically embedded. The arrangement was built so both could survive: kami cults gained protection and legitimation, Buddhism gained local purchase and revenue, and neither had to annihilate the other.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by critics across four centuries: Kitabatake Chikafusa's fourteenth-century complaint that Buddhism had subordinated the native kami; Yoshida Kanetomo's fifteenth-century anti-Buddhist polemics; Motoori Norinaga's eighteenth-century argument that kami worship needed no Buddhist frame; and the shrine-priest separation petitions of the early nineteenth century. The strongest corroboration of the problem's death is behavioral: when enforcement flipped in 1868, no institution defended the fusion at real cost to itself.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.60 for the arrangement's mature phase: shrine revenues and ritual authority flowed to temple administration, every household paid compulsory funerary fees, and the arrangement extracted something subtler — the ontological clarification that would have let kami institutions articulate a defense. Suppression (0.62) is structural and load-bearing: the terauke registration system was compulsory, purist movements were censored, and separation petitions were denied for two centuries before 1868. Theater (0.45 at the mature phase, rising through the interval) tracks the reading's core claim: the unity of kami and buddhas was performed ritually and diplomatically while the functional integration work atrophied — by the late Tokugawa the unity formula was invoked more often the less it bound anyone, crossing the Goodhart threshold around T=200. Accessibility_collapse (0.45) is moderate: alternatives (purist kami worship, independent shrines, sectarian Buddhism) existed throughout but were constrained, and the Meiji separation demonstrated they were executable once enforcement flipped. Resistance (0.55) was persistent — Yoshida polemic, kokugaku critique, Hirata's network, shrine-priest petitions — and was suppressed rather than answered. Fixing cost: for the bakufu, dismantling the arrangement was prohibitive — the temple registration system was load-bearing governance (census, anti-Christian surveillance), so the arrangement persisted not because it was cheap to keep but because it was expensive to remove; the cost class flipped only when the bakufu itself was destroyed. The measurement series run on one shared grid (T = years since 1600; eight points; all three metrics authored at every point). The terminal fall in base_extractiveness and suppression_requirement after T=268 models the enforcement frame dissolving with the bakufu — enforcement decay, not reform; the theater peak at T=268 is the last unity performance before the structure it decorated was gone.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the temple establishment's position the arrangement is stewardship it built and staffed — the jinguji kept shrines solvent and the kami ritually served; from the shrine priests' position it is subordination with a stipend; from the registered households' position it is an unavoidable fee apparatus attached to death; from the purists' position it is captivity; from the Meiji oligarchs' position it is cleared ground. The bakufu's seat is the most distinctive: it experienced the arrangement as governance infrastructure, which is why it enforced the fusion for two and a half centuries and why its fall — not any doctrinal argument — is what ended the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: the temple establishment collects the fees and administers the shrines (d near the beneficiary pole); the bakufu collects governance value and bears little of the arrangement's cost; the Meiji oligarchs collect the separation dividend — an unusual beneficiary whose gain flows from the arrangement's weakness rather than its operation, which the derivation handles through beneficiary-plus-arbitrage. Targets sit at the high-d end: registered households are trapped (registration was compulsory, affiliation transfer legally hazardous), purist movements are constrained (censored but organized), and shrine priests sit mid-to-high — they drew income from the fused economy while bearing subordination, a genuine dual position the secondary_role records. Scope is national, which amplifies effective extraction at the household seat: voluntary affiliation could not be verified at that scale, and the arrangement did not attempt to. Suppression is authored as a raw structural property (compulsion, censorship, denial of petitions) and is not scaled; only extraction is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating two religious institutional complexes without annihilating either — was solved by the end of the medieval period. The Tokugawa arrangement persisted on inertia and enforcement long after the integration work was done, which is the R5 signature: founding_problem_status dead paired with disappearance_verdict world_rearranges fires the mismatch flag, and the computed capture path (gain_flow to the temple establishment, prohibitive fixing cost while the bakufu stood) cross-checks it. The classification prevents two mislabelings: reading the arrangement as a pure snare would erase the real millennium-long coordination achievement (no sectarian war over the kami-buddha boundary, shared sacred economies that functioned); reading it as a pure rope would erase the compulsory extraction, the captured shrine economy, and the suppression of clarification. Tangled rope with terminal drift — enforcement ratchet through the Tokugawa period, theater accumulation past the Goodhart threshold, then enforcement collapse in 1868 — is the structure this reading asserts, and the temporal series is authored to show exactly that trajectory. The reading's distinctive contribution to mandatrophy analysis: a constraint whose kernel was never committed cannot mobilize committed defenders, so its obsolescence is not gradual atrophy but instantaneous collapse the moment enforcement flips — which is what the 1868-1870 record shows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the incoherence_reading of the shinbutsu_ontological_commitment kernel (no stable ontological commitment existed; the fusion was institutionally tolerated incoherence). How would the classification change under the sibling readings — syncretic_reading (kami and buddhas as aspects of one unified honji-suijaku order) and partition_reading (separate life-cycle and afterlife domains without ontological integration)?',
    'Compile the sibling readings as separate constraint files with their own epsilon, beneficiary/victim structures, and claimed types; compare per-seat classifications across the family. The disagreement is located in a single structural element the readings divide on: whether a binding ontological commitment underlay the arrangement.',
    'Under the syncretic reading the arrangement expresses a unified cosmological order (low extraction, coordination-dominant); under the partition reading it is two coexisting domains (near-zero extraction). Only under the incoherence reading does the enforced, hollow-kernel, extractive structure appear — the epsilon gap between siblings is itself the measurement the family exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-reading indexicality: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    commitment_depth_underdetermination,
    'Can the absence of stable ontological commitment be established from the surviving record, or does the record under-determine commitment — institutions may have held convictions they never needed to articulate because the arrangement never forced the ontological question?',
    'Close reading of doctrinal treatises, ritual commentaries, and dispute records for committed versus diplomatic invocation of honji-suijaku; look specifically for cases where actors bore real costs for ontological positions, which would evidence conviction rather than convenience.',
    'If genuine commitment existed, the speed of the Meiji collapse needs re-explanation, epsilon drops, and this reading converges toward the syncretic sibling; if incoherence is confirmed, the enforced-hollowness structure and the tangled-rope-to-terminal-collapse drift are vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_depth_underdetermination, empirical, 'Whether the historical record can settle the depth of ontological commitment.').

omega_variable(
    separation_ease_attribution,
    'Was the Meiji separation easy because no deep commitment existed (this reading''s causal claim), or would any arrangement have collapsed under the coercive monopoly the Meiji state deployed — the haibutsu kishaku destroyed thousands of temples regardless of what anyone believed?',
    'Compare institutional survival under comparable coercion across arrangements with documented commitment depth; examine differential resistance among Buddhist sects and among shrine-temple complexes served by committed versus careerist priestly lineages.',
    'If coercion alone explains the collapse, the kernel-instability premise weakens and the arrangement''s profile shifts toward pure enforcement-maintained extraction; if commitment depth predicts differential survival, the incoherence reading''s causal claim is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_ease_attribution, empirical, 'Attributing separation ease: kernel shallowness versus coercive asymmetry.').

omega_variable(
    meiji_benefit_status,
    'Does the Meiji state''s cheap separation count as a benefit the arrangement conferred (making the oligarchs a genuine beneficiary seat), or merely an opportunity the arrangement failed to block (an external shock rather than a seat in its structure)?',
    'Trace whether the arrangement''s structural properties — no committed defenders, institutional entanglement with the fallen bakufu, suppressed clarification tradition — causally enabled the cheap separation, or whether oligarch coercion would have succeeded at the same cost against a deeply committed arrangement.',
    'If the benefit is real, meiji_reform_oligarchs stays a beneficiary and the arrangement''s ledger includes subsidizing its own gravediggers; if not, the seat should be re-authored as excluded and the directionality profile tightens around the Tokugawa-era seats alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_benefit_status, conceptual, 'Whether the successor state''s gain is a benefit-flow of the standing arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 270).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(shin_tr_t50, observed).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(shin_tr_t100, observed).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 150, 0.44).
narrative_ontology:measurement_basis(shin_tr_t150, observed).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement_basis(shin_tr_t200, observed).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 250, 0.55).
narrative_ontology:measurement_basis(shin_tr_t250, observed).
narrative_ontology:measurement(shin_tr_t268, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 268, 0.62).
narrative_ontology:measurement_basis(shin_tr_t268, observed).
narrative_ontology:measurement(shin_tr_t270, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 270, 0.4).
narrative_ontology:measurement_basis(shin_tr_t270, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(shin_be_t50, observed).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(shin_be_t100, observed).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement_basis(shin_be_t150, observed).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement_basis(shin_be_t200, observed).
narrative_ontology:measurement(shin_be_t250, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 250, 0.66).
narrative_ontology:measurement_basis(shin_be_t250, observed).
narrative_ontology:measurement(shin_be_t268, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 268, 0.55).
narrative_ontology:measurement_basis(shin_be_t268, observed).
narrative_ontology:measurement(shin_be_t270, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 270, 0.3).
narrative_ontology:measurement_basis(shin_be_t270, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(shin_su_t50, observed).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(shin_su_t100, observed).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 150, 0.66).
narrative_ontology:measurement_basis(shin_su_t150, observed).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement_basis(shin_su_t200, observed).
narrative_ontology:measurement(shin_su_t250, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 250, 0.64).
narrative_ontology:measurement_basis(shin_su_t250, observed).
narrative_ontology:measurement(shin_su_t268, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 268, 0.3).
narrative_ontology:measurement_basis(shin_su_t268, observed).
narrative_ontology:measurement(shin_su_t270, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 270, 0.15).
narrative_ontology:measurement_basis(shin_su_t270, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, meiji_shinbutsu_bunri_edicts).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, state_shinto_formation).

% DUAL FORMULATION NOTE:
% The label 'shinbutsu-shugo' conflates three structurally distinct claims about the kami-buddha relationship: a unified cosmological order (syncretic_reading), separate functional domains without ontological integration (partition_reading), and no stable ontological commitment at all (this file, incoherence_reading). Per the epsilon-invariance principle each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification; they are linked here as a constraint family. This file carries the highest epsilon of the three: on the incoherence reading the arrangement's coordination was real but shallow, its enforcement load-bearing, and its kernel hollow — which is why the Meiji separation met so little ontological resistance. The sibling files should document their own epsilon values and cite this decomposition; the downstream files (separation edicts, State Shinto formation) inherit this reading's structural prediction that separation would be cheap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
