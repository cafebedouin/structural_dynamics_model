% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Revelation Extended by Disciplined Reason
 *   domain: religious/legal/institutional-history
 *
 * SUMMARY:
 *   The Hanafi methodological regime binds law to Qur'an and Hadith but
 *   routes every hard case through a rationalist filter: qiyas (analogy
 *   locating an operative cause in the texts and extending it) and istihsan
 *   (juristic preference, permitting a trained jurist to set aside a strict
 *   analogy whose outcome harms). Reason is thereby licensed as a legitimate
 *   instrument of divine intent, and with it a professional class whose
 *   decade-long training is the only credential that makes legal reasoning
 *   count. The regime solved a real problem — a scripture-bound community
 *   facing constant novelty needed principled extension — while concentrating
 *   law-finding authority in that class and displacing the textualist claim
 *   that transmitted text alone speaks for God. This file instantiates ONE
 *   reading of the jurisprudential_method_kernel (see
 *   commentary.kernel_context and the omega variables for the committer
 *   structure); the epsilon authored here refers to the standing Hanafi
 *   arrangement as it operates, assessed from within the reading's own
 *   commitments — the extraction a committed participant could see: rivals
 *   losing institutional ground, laypeople unable to verify rulings issued
 *   under divine warrant, authority pooling in the trained. Claim and metrics
 *   are independent authored facts: claimed_type records the structure I
 *   believe true (tangled_rope — real coordination, real asymmetry, actively
 *   enforced); the metrics record the operation I believe descriptively
 *   accurate; the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurists: primary beneficiary and agenda-setter (organized/identity_locked) — runs the method and collects interpretive authority
 *   - imperial_administrations: secondary beneficiary (institutional/mobile) — patronizes the school for governability
 *   - hadith_traditionist_scholars: primary target (organized/identity_locked) — bears displacement of the exclusive-textual claim
 *   - rival_madhhab_jurists: secondary target (organized/constrained) — loses adjudication share in Hanafi territories
 *   - lay_muslim_litigants: dual-positioned (powerless/trapped) — receives adaptive law it cannot verify
 *   - qadis_judicial_officers: operational beneficiary (moderate/constrained)
 *   - unmediated_lay_readers: excluded voice (powerless/trapped) — untrained readings carry no standing
 *   - comparative_law_historians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.56).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.4).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Revelation Extended by Disciplined Reason").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious/legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '8709b916-e735-4b5e-a9f7-fb3e05549239').
narrative_ontology:cs_kernel_codification('8709b916-e735-4b5e-a9f7-fb3e05549239', fixed_text).
narrative_ontology:cs_authority_grounding('8709b916-e735-4b5e-a9f7-fb3e05549239', lineage).
narrative_ontology:cs_interpretation_layer_present('8709b916-e735-4b5e-a9f7-fb3e05549239').
narrative_ontology:cs_reading_relation('8709b916-e735-4b5e-a9f7-fb3e05549239', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('8709b916-e735-4b5e-a9f7-fb3e05549239', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('8709b916-e735-4b5e-a9f7-fb3e05549239', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('8709b916-e735-4b5e-a9f7-fb3e05549239', foundational, disciplined_reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(disciplined_reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('8709b916-e735-4b5e-a9f7-fb3e05549239', disciplined_reason_extends_divine_intent, instrumental).
narrative_ontology:cs_axiom('8709b916-e735-4b5e-a9f7-fb3e05549239', foundational, equity_discretion_is_valid_law_finding).
narrative_ontology:cs_axiom_status(equity_discretion_is_valid_law_finding, holdable).
narrative_ontology:cs_axiom_grounding('8709b916-e735-4b5e-a9f7-fb3e05549239', equity_discretion_is_valid_law_finding, conventional).
narrative_ontology:cs_reference_frame('8709b916-e735-4b5e-a9f7-fb3e05549239', revelation_extended_by_disciplined_reason).
narrative_ontology:cs_drift_state('8709b916-e735-4b5e-a9f7-fb3e05549239', contemporary_nation_state_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8709b916-e735-4b5e-a9f7-fb3e05549239', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, imperial_administrations).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, hadith_traditionist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, rival_madhhab_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, lay_muslim_litigants).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, qadis_judicial_officers).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslim_litigants).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_legitimate_in_divine_law).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, analogical_extension_of_revelation).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, equity_discretion_istihsan).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train for a decade or more in Arabic philology, hadith criticism, and usul al-fiqh before their reasoning carries weight; staff and run the seminaries, commentary traditions, and fatwa councils that decide which analogies count. Status, livelihood, and influence flow from interpretive standing. Leaving the method means repudiating the training that constitutes their authority — the exit is not a door but a demolition of a life's credential.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, agenda_setter).

% Abbasid and Ottoman rulers appointed Hanafi chief qadis and built court systems on the school's doctrine because a reason-extensible law could absorb novel fiscal, commercial, and administrative questions without waiting for textual warrant. Patronage could shift to another school if usefulness lapsed, and dynasties occasionally did exactly that.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, imperial_administrations, beneficiary,
    institutional, generational, mobile, continental).

% Scholars of the ahl al-hadith movement hold that God's law reaches the community only through transmitted text and unanimous consensus. Where the Hanafi method dominates, their rulings are rejected in court, their students are recruited away by better-endowed rationalist seminaries, and their claim to speak exclusively for revelation loses institutional ground. Fidelity to text is their identity; adopting analogical method would dissolve the very claim that defines them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hadith_traditionist_scholars, payer,
    organized, generational, identity_locked, continental).

% Maliki, Shafi'i, and Hanbali jurists in Hanafi-dominant territories keep their scholarly freedom but find state courts applying Hanafi doctrine; litigants shop between schools for favorable rulings, and the rivals' share of actual adjudication shrinks. Relocating to another school's territory is possible but costly — libraries, networks, and reputation are local.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rival_madhhab_jurists, payer,
    organized, generational, constrained, continental).

% Receive determinate answers on marriage, commerce, inheritance, and ritual that rigid textualism could not supply — credit instruments, partnership forms, questions the texts never faced. They cannot independently verify a ruling's reasoning; answers arrive with divine authority attached although produced by fallible human inference, and they cannot shop for another law.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslim_litigants, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, lay_muslim_litigants, payer).

% Judges receive a workable decision procedure: doctrine rich enough to answer the docket and a hierarchy of authorities to cite. They bear the workload of applying dense analogical doctrine and the career risk of rulings overturned on review by senior jurists.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, qadis_judicial_officers, beneficiary,
    moderate, biographical, constrained, regional).

% Believers who read the Qur'an and Hadith directly and form their own view of what God requires. Under the school's regime their untrained reading carries no standing in any court or fatwa council; participation in law-finding requires credentials they have no realistic path to obtain late in life.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, unmediated_lay_readers, excluded,
    powerless, biographical, trapped, regional).

% Historians of Islamic law reconstruct how the four Sunni schools' methods formed, competed, and were institutionalized; they see the whole surface — which disputes were about method, which about patronage, and where the reasoning apparatus did and did not track the doctrine it justified.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends revealed law to cases the texts do not address — partnership forms, credit instruments, procedural and familial novelty — so a trans-regional community has determinate answers without new revelation, and standardizes law-finding across a vast territory through one shared method.
% TRANSFER_FUNCTION: Moves law-finding authority — and the status, livelihood, and adjudication control attached to it — from untrained readers and rival-method scholars to the trained jurist class; every novel question's answer-right transfers to whoever masters qiyas and istihsan, with the ruling issued under divine rather than human warrant.
% ABSENT_VOICES: Unmediated lay readers of the primary texts would object that their direct readings carry no standing anywhere in the system; in Hanafi-dominant states, non-Hanafi litigants before Hanafi courts likewise had no seat. They sit outside the seminary system, without the decade-plus training that would give their reasoning force.
% DISAPPEARANCE_RATIONALE: Marriages, contracts, inheritances, and endowments across Hanafi-majority societies are structured through doctrines this method produced; overnight removal would strip courts and fatwa organs of their justificatory apparatus, strand pending litigation, and dissolve the jurist class's function — the legal world would rearrange around whichever replacement method the surviving institutions adopted.
% FOUNDING_PROBLEM: A community committed to living by divine law constantly meets cases the revelation never addressed; it needed a principled way to reach answers that neither froze the law nor admitted arbitrary human legislation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: all three rival schools built their own extension mechanisms (Maliki practice-orientation, Shafi'i's subordinate qiyas tier, even restricted Hanbali devices), which is inexplicable if the problem were a jurist-class invention; contemporary minority-community demand for rulings on finance and bioethics attests the problem's liveness from beyond the traditional beneficiary set. What remains contested — and the textualist parties say so explicitly — is whether this solution's discretionary breadth is necessary.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56: the method genuinely answers novel cases (coordination) while pooling law-finding in a credentialed class and issuing human inference under divine warrant (asymmetry); the value sits well above a rope's incidental overhead and below a snare's totalizing capture because the service delivered is real and the classical jurist bore heavy training costs rather than collecting fees. Suppression 0.40 is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Theater 0.42: the apparatus functions where practiced (Diyanet, fiqh academies, Deobandi ifta councils), but a large contemporary share of madhhab methodological activity is curricular rehearsal and identity performance; held under 0.5 because operative organs persist. Accessibility_collapse 0.50: inside the framework the pure-textualist alternative collapses (one cannot unlearn that reason extends revelation), while the three sibling schools remain live outside it. Resistance 0.65: twelve centuries of sustained textualist opposition — the ahl al-hadith movement, al-Shafi'i's attack on istihsan, Ibn Taymiyya, modern anti-taqlid Salafism. Identity-lock binds BOTH poles: the jurist's professional self is constituted by methodological mastery (exit demolishes a life's credential), the traditionist's ideological self by fidelity to text (exit dissolves the exclusive-authenticity claim); break either frame and the corresponding seat's computed classification shifts materially. The measurement series share one grid (750, 950, 1258, 1550, 1924, 2024) with all three metrics authored at every point. suppression_requirement is tracked because enforcement capacity genuinely arcs: polemical formative enforcement, guild consolidation under taqlid norms, state-backed Ottoman hardening, then collapse after the caliphate's abolition — a rise-and-decay trajectory, not a cycle. base_extractiveness peaks at the Ottoman codification (doctrine frozen into state law, maximal warrant behind narrowed discretion) and decays as European-style codes displace fiqh courts.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the senior jurist's position the regime is a disciplined science it staffs and transmits — coordination it built. From the traditionist's position the same structure is sanctioned innovation that strips text of exclusive authority. From the litigant's position it is simply authoritative answers arriving with God's signature on human inference. From the historian's position the method's shape tracks patronage as much as piety. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   rationalist_trained_jurists declare as beneficiaries with identity_locked exit: derivation places them near the full-beneficiary end — the regime subsidizes exactly their credential. imperial_administrations join as beneficiaries with mobile (arbitrage-grade) exit: they patronized whichever school governed well and could switch, sitting nearest the beneficiary pole. hadith_traditionist_scholars and rival_madhhab_jurists declare as victims; the former's identity_locked exit pushes them toward the full-target end, the latter's constrained exit leaves them somewhat below it. lay_muslim_litigants are dual-declared (beneficiary of adaptivity, payer of unverifiability) and should derive near-symmetric; qadis derive mildly beneficiary. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct qualitative placement for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reaching principled answers for cases the texts never faced — remains live wherever the community treats revelation as governing: novel finance, bioethics, and digital transactions regenerate demand continuously, and the rival schools' parallel extension mechanisms corroborate the problem from outside the beneficiary set. Mandatrophy is therefore NOT resolved: the mandate has not outlived its function. The classification disciplines two mislabelings at once: reading the regime as pure extraction (snare) ignores that novel-case resolution is a genuine collective need no community of revelation can decline; reading it as pure coordination (rope) ignores that the same structure pools authority in a credentialed class and strips rivals' claims of standing. Tangled rope names both halves; the theater series guards against a future drift-toward-piton verdict should the functioning organs continue to shrink.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading (hanafi_reading) of the jurisprudential_method_kernel; which structural facts would change under the sibling readings?',
    'Comparative generation of the maliki_reading, shafii_reading, and hanbali_reading files; diff their beneficiary/victim sets, epsilon values, and computed types against this story.',
    'Under hanbali_reading the analogical-extension layer vanishes and the jurist discretionary space that generates this story''s extraction closes; under maliki_reading the supplement is communal practice rather than jurist reason, relocating the beneficiary seat from trained jurists to the custodians of Medinan tradition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of a four-reading kernel.').

omega_variable(
    disagreement_location_method_status,
    'Where exactly do the readings disagree — what is the contested element the sibling set partitions on?',
    'Locate the dispute axis: whether anything besides transmitted text may supplement revelation, and if so whether it is jurist reason (hanafi), communal practice (maliki), a strictly subordinated fourth tier (shafii), or nothing short of unanimous consensus (hanbali).',
    'The source of measured extraction changes across readings: hanafi locates it in discretionary reason, hanbali denies the layer exists at all, shafii subordinates it until extraction is minimal by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_method_status, conceptual, 'Locus of inter-reading disagreement: the status of supplementary sources beyond the texts.').

omega_variable(
    istihsan_vs_qiyas_extraction_split,
    'How much of the measured extraction flows from istihsan''s equity discretion versus qiyas''s disciplined analogy?',
    'Doctrinal audit separating rulings reached by strict analogy (operative cause identifiable in the texts) from rulings reached by juristic preference overriding analogy; al-Shafi''i''s attack targeted the latter category specifically.',
    'If most extraction rides on istihsan, the shafii_reading sibling (which rejects istihsan but keeps qiyas) computes materially lower epsilon; if qiyas itself carries it, all reason-permitting readings share the load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_vs_qiyas_extraction_split, empirical, 'Splitting the extraction attributable to the two rationalist tools.').

omega_variable(
    taqlid_freeze_confound,
    'Does the measured extraction reflect the method itself or the medieval taqlid norm that barred individual jurists from independent reasoning within the school?',
    'Compare extraction profiles of classical taqlid-era Hanafism against modern ijtihad-revival movements operating the same methodological toolkit.',
    'If taqlid is the confound, the method''s own epsilon is lower than measured and the concentration belongs to a separate guild-structure constraint; revival movements would then reduce rather than increase extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_freeze_confound, empirical, 'Whether guild closure (taqlid) rather than method generates the authority concentration.').

omega_variable(
    divine_authority_cloak_status,
    'Is issuing humanly-reasoned rulings under divine authority an extraction this reading''s structure creates, or an intrinsic feature of any revealed-law system that no reading of this kernel removes?',
    'Compare against a fully naturalized legal system of comparable scope: if the authority-cloak effect persists wherever law claims transcendent warrant, it is kernel-level rather than reading-level.',
    'If kernel-level, all four sibling stories carry a common extraction floor and cross-reading epsilon differences measure only each method''s marginal effect; if reading-level, the hanafi reading owns a distinctive share proportional to its discretionary surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_authority_cloak_status, conceptual, 'Whether the divine-warrant cloak over human reasoning is removable within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 750, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t750, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement(juri_tr_t950, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 950, 0.22).
narrative_ontology:measurement(juri_tr_t1258, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1258, 0.25).
narrative_ontology:measurement(juri_tr_t1550, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1550, 0.35).
narrative_ontology:measurement(juri_tr_t1924, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1924, 0.38).
narrative_ontology:measurement(juri_tr_t2024, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(juri_be_t750, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 750, 0.48).
narrative_ontology:measurement(juri_be_t950, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 950, 0.58).
narrative_ontology:measurement(juri_be_t1258, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1258, 0.63).
narrative_ontology:measurement(juri_be_t1550, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1550, 0.67).
narrative_ontology:measurement(juri_be_t1924, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1924, 0.6).
narrative_ontology:measurement(juri_be_t2024, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 2024, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t750, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 750, 0.55).
narrative_ontology:measurement(juri_su_t950, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 950, 0.6).
narrative_ontology:measurement(juri_su_t1258, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1258, 0.62).
narrative_ontology:measurement(juri_su_t1550, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement(juri_su_t1924, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1924, 0.45).
narrative_ontology:measurement(juri_su_t2024, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic jurisprudential method' decomposes, per the epsilon-invariance principle, into four structurally distinct regimes sharing one fixed-text core (Qur'an/Hadith authority) and diverging on the extension layer. This file instantiates the hanafi_reading; maliki_reading, shafii_reading, and hanbali_reading are separate constraints with their own epsilon, beneficiary/victim sets, and classifications. The shared textual core is upstream common ground; each reading's extension layer is where extraction diverges — hanafi licenses the broadest jurist discretion of the four, hanbali denies the layer outright, shafii subordinates it to transmitted hadith, maliki reroutes it through communal practice. The links here are constraint-family membership edges, not causal claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
