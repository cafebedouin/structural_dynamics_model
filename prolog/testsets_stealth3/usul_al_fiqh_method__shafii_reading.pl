% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Source Hierarchy: Authentication-Gated Legal Derivation
 *   domain: religious/legal-theory/comparative-law
 *
 * SUMMARY:
 *   Al-Shafi'i's systematization of usul al-fiqh (al-Risala, al-Umm, early
 *   9th century) fixed an ordered hierarchy of legal sources: verified Quran,
 *   authenticated Sunna, the consensus of the Companions, and analogical
 *   reasoning only where verified texts are silent. The arrangement solved a
 *   real coordination problem — legal derivation had fragmented across
 *   regions using unverified reports, personal reasoning, and local custom
 *   with no shared procedure — while simultaneously transferring the power to
 *   make a report legally usable from jurists of the Iraqi reasoned-opinion
 *   tradition to the specialists who certify transmission chains. This file
 *   is ONE READING of the kernel usul_al_fiqh_method: the shafii_reading. The
 *   hanafi, maliki, and hanbali readings are separate constraints (separate
 *   files, linked via network.affects_constraints); nothing about them is
 *   averaged into this story. Per the epsilon-referent rule for
 *   kernel-reading stories, the referent of the authored epsilon is the
 *   standing Shafi'i arrangement itself, assessed by this reading's own
 *   lights — the reading endorses the hierarchy as fidelity to revelation, so
 *   it scores the arrangement's costs as real but largely corrective, not as
 *   rent. The claim (tangled_rope) and the metrics are independent authored
 *   facts: the reading would call the structure legitimate coordination with
 *   a necessary price; the metrics describe the actual asymmetric authority
 *   transfer the historical record shows.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: primary beneficiary (organized/constrained) — certification of reports through transmission chains becomes the gateway through which all legal derivation must pass; standing, stipends, and teaching posts flow to them
 *   - shafii_madhhab_establishment: agenda-setter (institutional/mobile) — administers the meta-discipline, licenses jurists, adjudicates method disputes, and collects prestige, endowments, and appointment influence
 *   - ahl_al_ray_jurists: primary target (powerful/identity_locked) — the Iraqi reasoned-opinion tradition bears the demotion of analogy and the banning of juristic preference; their authority is fused with their school lineage
 *   - local_practice_jurists: secondary target (moderate/constrained) — jurists relying on inherited communal practice bear reclassification of their rulings as error or innovation absent textual warrant
 *   - appointed_qadis: dual-positioned (institutional/constrained) — gain a standardized, defensible procedure; pay when textual strictness blocks pragmatic settlements
 *   - lay_muslim_questioners: diffuse beneficiary-payer (powerless/trapped) — receive more consistent and verifiable rulings; bear narrowed local adaptation; cannot exit the law's jurisdiction
 *   - rival_madhhab_jurists: excluded competitors (organized/mobile) — hold the Hanafi, Maliki, and Hanbali frameworks; contest the closure claims from outside this framework's internal conversation
 *   - comparative_legal_historians: analytical observer — reconstruct the formation of the discipline and weigh forgery evidence against authority-building effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.48).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.5).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Source Hierarchy: Authentication-Gated Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious/legal-theory/comparative-law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '1b498c39-33b7-4431-977e-cf7e61c2cfa5').
narrative_ontology:cs_kernel_codification('1b498c39-33b7-4431-977e-cf7e61c2cfa5', formalized).
narrative_ontology:cs_authority_grounding('1b498c39-33b7-4431-977e-cf7e61c2cfa5', lineage).
narrative_ontology:cs_interpretation_layer_present('1b498c39-33b7-4431-977e-cf7e61c2cfa5').
narrative_ontology:cs_reading_relation('1b498c39-33b7-4431-977e-cf7e61c2cfa5', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('1b498c39-33b7-4431-977e-cf7e61c2cfa5', usul_al_fiqh_method__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('1b498c39-33b7-4431-977e-cf7e61c2cfa5', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('1b498c39-33b7-4431-977e-cf7e61c2cfa5', foundational, authenticated_hadith_prerequisite_for_derivation).
narrative_ontology:cs_axiom_status(authenticated_hadith_prerequisite_for_derivation, holdable).
narrative_ontology:cs_axiom_grounding('1b498c39-33b7-4431-977e-cf7e61c2cfa5', authenticated_hadith_prerequisite_for_derivation, theological).
narrative_ontology:cs_axiom('1b498c39-33b7-4431-977e-cf7e61c2cfa5', foundational, qiyas_subordinate_to_textual_presence).
narrative_ontology:cs_axiom_status(qiyas_subordinate_to_textual_presence, holdable).
narrative_ontology:cs_axiom_grounding('1b498c39-33b7-4431-977e-cf7e61c2cfa5', qiyas_subordinate_to_textual_presence, instrumental).
narrative_ontology:cs_axiom('1b498c39-33b7-4431-977e-cf7e61c2cfa5', secondary, companions_consensus_exclusivity).
narrative_ontology:cs_axiom_status(companions_consensus_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('1b498c39-33b7-4431-977e-cf7e61c2cfa5', companions_consensus_exclusivity, empirically_contingent).
narrative_ontology:cs_reference_frame('1b498c39-33b7-4431-977e-cf7e61c2cfa5', authenticated_textual_source_hierarchy).
narrative_ontology:cs_drift_state('1b498c39-33b7-4431-977e-cf7e61c2cfa5', post_classical_taqlid_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b498c39-33b7-4431-977e-cf7e61c2cfa5', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_madhhab_establishment).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, ahl_al_ray_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, local_practice_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, appointed_qadis).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, lay_muslim_questioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, appointed_qadis).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, lay_muslim_questioners).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, isnad_authentication_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, textual_supremacy_over_reason).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, prophetic_precedent_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Travel, gather, and grade reports of the Prophet's words and deeds by examining the reliability of each transmission chain. Their certification decides which reports may enter legal reasoning at all. Teaching posts, stipends, and scholarly standing flow to them because every jurist now needs their verdict before deriving a rule. Leaving the trade would mean abandoning the craft that defines their standing; few do.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    organized, generational, constrained, continental).

% Runs the methodological discipline: teaches the Risala and the later usul manuals, licenses jurists through teaching chains, adjudicates disputes over method, and supplies jurists to judicial posts. Prestige, endowment income, and appointment influence accrue to its lineages. Its leading members relocate between Baghdad, Cairo, and Damascus carrying their authority with them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_madhhab_establishment, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, shafii_madhhab_establishment, beneficiary).

% The Iraqi juristic tradition descending from Abu Hanifa, whose standing rested on disciplined reasoned opinion, systematic analogy, and juristic preference where texts ran thin. Under the new hierarchy their instruments are demoted: analogy only where no sound report exists, preference dismissed as unlawful discretion, their consensus claims ruled out by the restriction to the Companions' generation. Their authority is fused with their school's lineage — abandoning the method would mean surrendering the identity and livelihood the tradition confers, so they defend it instead.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, ahl_al_ray_jurists, payer,
    powerful, biographical, identity_locked, continental).

% Jurists in regions such as Medina and Syria whose rulings long rested on inherited communal practice and customary law. The hierarchy reclassifies practice without textual warrant as error or innovation, stripping their rulings of authority unless re-derived through certified reports. Adapting is possible but costly: their local knowledge does not translate into transmission-chain expertise.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, local_practice_jurists, payer,
    moderate, biographical, constrained, regional).

% Judges appointed by rulers who increasingly draw on the systematized method. They gain a standardized, defensible procedure — rulings traceable to recognized sources protect them from challenge. They pay when textual strictness blocks pragmatic settlements their communities need, forcing a choice between procedure and local peace.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, appointed_qadis, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, appointed_qadis, payer).

% Ordinary believers asking for rulings on worship, marriage, commerce, and injury. They receive more consistent and verifiable answers across regions and can in principle ask what source a ruling rests on. They bear the narrowing of locally adapted answers and cannot exit the law's jurisdiction over their lives.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, lay_muslim_questioners, beneficiary,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, lay_muslim_questioners, payer).

% Jurists holding the Hanafi, Maliki, and Hanbali frameworks. They contest the closure of the source list and the restriction of consensus, defending their own hierarchies in disputation and teaching. They stand outside this framework's internal conversation; the persistence of their schools marks the boundary of its authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rival_madhhab_jurists, excluded,
    organized, generational, mobile, continental).

% Modern scholars reconstructing how the discipline formed, weighing the documented forgery epidemic against the authority-building effects of the certification gate, and comparing the four schools' divergent trajectories. They hold no stakes in any school's standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of divergent legal derivation: before a shared hierarchy, jurists in different regions issued conflicting rulings from unverified reports, personal reasoning, and local custom with no agreed procedure. The arrangement fixes an ordered source list — verified Quran, authenticated Sunna, the Companions' consensus, then analogy — so jurists trained anywhere can reproduce how a ruling was reached and check each other's work.
% TRANSFER_FUNCTION: Moves interpretive authority from jurists whose standing rests on reasoned opinion and inherited local practice toward the specialists who certify reports through transmission chains; concretely, the power to make a report legally usable passes to the authentication experts, and with it teaching posts, stipends, and judicial influence.
% ABSENT_VOICES: The rationalist jurists of the Iraqi school objected vigorously in life but enter this framework's written self-record mainly as defeated opponents; women subject to the rulings had no seat in methodological debate; Shi'i and Khariji legal traditions were excluded from the conversation entirely; non-Arab communities living under customs the hierarchy reclassified as innovation had no advocate in it.
% DISAPPEARANCE_RATIONALE: If the authentication-gated hierarchy vanished overnight, legal derivation would fragment back into regional practice and reasoned opinion; the transmission specialists' certification role would lose its legal purchase; the four-school map would redraw around whichever methods filled the vacuum; rulings currently traceable to named sources would lose their audit trail.
% FOUNDING_PROBLEM: Reports attributed to the Prophet circulated with fabricated or broken chains while jurists issued binding rulings from unexamined material and personal preference; al-Shafi'i's stated aim was to make verified prophetic precedent the non-negotiable ground of legal derivation and to give the community one reproducible method.
% FOUNDING_PROBLEM_CORROBORATION: Hadith critics outside any Shafi'i allegiance attest the fabrication problem: Muslim ibn al-Hajjaj's introduction to his Sahih documents the forgery epidemic, and Ibn al-Salah's later manual codifies the crisis independently of school loyalty. Modern academic historians (Schacht; Motzki and the isnad-cum-matn school) corroborate that the problem was real while disputing whether the Shafi'i solution matched it — some argue verification practice predates and exceeded the Risala's use of it. Corroboration for the problem's reality is broad and external to the benefiting parties; corroboration for the arrangement as the right or still-needed answer is genuinely contested.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (reading-indexed): the reading itself documents the displacement of reasoned-opinion authority as real — al-Shafi'i's polemic against ahl al-ra'y and istihsan was aggressive — but regards most of that cost as necessary correction rather than rent, which holds the value below what an outside critic would assign. Suppression is 0.50 and is a raw structural property, unscaled by power or scope: enforcement ran through disputation, ijaza (teaching-license) control, and appointment influence rather than coercive force, and rival madhhabs persisted externally, capping it below the levels seen in state-enforced arrangements. Theater is low (0.18): isnad criticism performed genuine filtering work — fabricated reports were a documented epidemic — though a growing share of activity became credentialing ritual as usul mastery turned into the entrance examination for scholarly standing. Accessibility_collapse (0.60) reflects that alternatives collapse almost completely inside the framework once accepted (deriving law from unauthenticated material simply stops being available) while external madhhab alternatives persist. Resistance (0.62) is high: the ra'y school defended its instruments for generations, and al-Shafi'i himself met fierce opposition in Baghdad and Egypt. The measurement series run on ONE shared six-point grid (T=0..30) with all three metrics authored at every point; the rising suppression_requirement series is authored deliberately because the story specifically tracks enforcement-capacity change — machinery matured from open disputation toward institutionalized license-control and appointment politics, plateauing once the madhhab structure consolidated. The extractiveness series rises through consolidation and plateaus rather than cycling: no oscillation mechanism is present, so no cyclical commentary applies. Identity-lock dynamics: the ahl_al_ray_jurists seat is authored identity_locked because their method was fused with lineage identity — Abu Hanifa's inherited Iraqi tradition — such that abandoning reasoned opinion meant surrendering the accumulated authority their school conferred; had that fusion broken (as it partially did when individual jurists changed affiliation), resistance would have collapsed faster and the suppression series would flatten earlier.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by design. From the agenda-setter seat (shafii_madhhab_establishment), the arrangement is a discipline it built and staffs: a reproducible method, an examinable curriculum, a portable authority. From the primary target seat (ahl_al_ray_jurists), the same structure operates as the demotion of everything their tradition stood on — analogy demoted to last resort, preference banned outright, consensus redefined to exclude their claims. The appointed_qadis seat straddles: procedural defensibility gained, pragmatic flexibility lost. The engine computes these per-seat classifications from the structural data (role, power, exit, scope); the authored claim does not adjudicate between them. Coalition potential among targets was limited: the ra'y jurists were individually powerful but regionally concentrated, and the practice-jurist class was diffuse and lacked trans-regional organization — which is part of why resistance, though prolonged, never overturned the hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive low directionality: hadith_transmission_specialists sit near the beneficiary end (the gate subsidizes them directly — their verdict is now a precondition for all derivation), and shafii_madhhab_establishment collects administrative rents while running the system. Declared victims drive high directionality: ahl_al_ray_jurists bear the transfer with identity_locked exit (trapped-or-locked targets sit nearer the full-target end), and local_practice_jurists bear delegitimation with merely constrained exit. appointed_qadis sit near symmetric — genuine procedural benefit against real flexibility cost. lay_muslim_questioners lean beneficiary (consistency, verifiability) with trapped exit amplifying whatever cost reaches them. rival_madhhab_jurists are excluded rather than coordinated: their exclusion from the source list is part of what the enforcement defends. Continental spatial scope moderately amplifies effective extraction for targets (verification of transmission claims across the Islamic ecumene was hard), which the engine scales; suppression stays unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Calling this a snare would erase the genuine coordination function: a shared, auditable source hierarchy really did solve the fragmentation problem, and the authentication discipline really did filter fabricated reports — the founding problem was attested by hadith critics far outside any Shafi'i allegiance. Calling it a rope would erase the asymmetric extraction: the same structure that coordinated derivation also stripped a specific class of jurists of their authority basis and redirected the power to certify toward a specific guild, enforced actively through polemic, license control, and appointment politics — hence tangled_rope with requires_active_enforcement. On mandatrophy: the founding problem (unverified reports corrupting law) was never fully dead — fabrication persisted for centuries — but the arrangement's center of gravity shifted from solving it to administering credentials around it; the founding_problem_status is therefore authored contested, and the status-by-verdict pair (contested x world_rearranges) should not fire the zombie flag, correctly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This story instantiates one reading (shafii_reading) of the kernel usul_al_fiqh_method; where exactly is the disagreement among the four readings located?',
    'Comparative analysis of the four readings'' source lists and authority maps: hanafi (expansive qiyas, istihsan), maliki (Medinan practice, maslaha mursala), hanbali (maximal textual restriction, weak reports over analogy), shafii (authentication-gated closed hierarchy). The disagreement sits in the closure of the source hierarchy — what counts as a source, and who certifies entry.',
    'If the disagreement is located primarily in authentication authority rather than source closure, the beneficiary/victim structure shifts from jurist-versus-transmitter toward a broader credentialing contest among all scholarly guilds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Location of the inter-reading disagreement within the usul al-fiqh kernel.').

omega_variable(
    forgery_response_vs_authority_building,
    'Was the authentication prerequisite primarily an epistemic response to the documented fabrication of prophetic reports, or an authority-building move that used the forgery crisis as justification?',
    'Chronological and geographic analysis: whether authentication infrastructure expanded where forgery was worst or where transmitter networks were strongest; comparison of pre-Risala verification practice in Medina and Kufa with the requirements al-Shafi''i codified.',
    'A primarily epistemic origin strengthens the coordination half of the tangled_rope reading; a primarily political origin shifts weight toward the extraction half and toward snare-flavored drift over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forgery_response_vs_authority_building, empirical, 'Whether the authentication gate originated as forgery defense or as guild authority construction.').

omega_variable(
    victim_class_dissolution,
    'The rationalist-opinion jurists the hierarchy displaced largely assimilated or faded within a few centuries — does the standing arrangement still impose costs on a living victim class, or has the asymmetry decayed into credentialing overhead?',
    'Track who bears measurable costs under the mature madhhab system: jurists disciplined for method deviation, regions where customary rulings were suppressed, scholars blocked from advancement for lacking transmission credentials.',
    'If the victim class has dissolved, the arrangement drifts from tangled_rope toward rope (coordination with legacy costs) or piton (inertia-maintained); if costs persist in the credentialing economy, extraction remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_dissolution, empirical, 'Persistence of a living cost-bearing class beneath the authentication gate.').

omega_variable(
    risala_vs_madhhab_referent,
    'Is the standing arrangement under assessment the Risala''s direct-derivation hierarchy (each jurist derives from the sources, taqlid condemned) or the later madhhab culture of taqlid and credentialing that actually operated for most of the interval?',
    'Date when direct derivation ceased to be practiced by most working jurists and taqlid became the operative norm; assess the arrangement separately at each phase before assigning a single epsilon.',
    'If the referent is the taqlid-era arrangement, measured extraction reflects credentialing rents rather than the Risala''s design; the theater_ratio series, the drift_state declaration, and the epsilon value all depend on this choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risala_vs_madhhab_referent, conceptual, 'Epsilon-referent ambiguity between the founder''s design and the operating madhhab system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t6, usul_al_fiqh_method__shafii_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement_basis(usul_tr_t6, observed).
narrative_ontology:measurement(usul_tr_t12, usul_al_fiqh_method__shafii_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(usul_tr_t12, observed).
narrative_ontology:measurement(usul_tr_t18, usul_al_fiqh_method__shafii_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement_basis(usul_tr_t18, observed).
narrative_ontology:measurement(usul_tr_t24, usul_al_fiqh_method__shafii_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(usul_tr_t24, observed).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__shafii_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(usul_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t6, usul_al_fiqh_method__shafii_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(usul_be_t6, observed).
narrative_ontology:measurement(usul_be_t12, usul_al_fiqh_method__shafii_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(usul_be_t12, observed).
narrative_ontology:measurement(usul_be_t18, usul_al_fiqh_method__shafii_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement_basis(usul_be_t18, observed).
narrative_ontology:measurement(usul_be_t24, usul_al_fiqh_method__shafii_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(usul_be_t24, observed).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__shafii_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(usul_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t6, usul_al_fiqh_method__shafii_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(usul_su_t6, observed).
narrative_ontology:measurement(usul_su_t12, usul_al_fiqh_method__shafii_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(usul_su_t12, observed).
narrative_ontology:measurement(usul_su_t18, usul_al_fiqh_method__shafii_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement_basis(usul_su_t18, observed).
narrative_ontology:measurement(usul_su_t24, usul_al_fiqh_method__shafii_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(usul_su_t24, observed).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__shafii_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(usul_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'usul al-fiqh' covers four structurally distinct readings of one kernel, each with its own source list, beneficiary/victim structure, and epsilon. This file instantiates the shafii_reading (authentication-gated closed hierarchy; beneficiaries: transmission specialists; victims: reasoned-opinion jurists). The hanafi_reading inverts the gate (analogy expansive, preference valid), the maliki_reading widens the source list (practice and public interest), and the hanbali_reading tightens it (weak reports over analogy). Edges run from this story to all three siblings; the upstream epistemic discipline of isnad authentication conditions all four and is cited as evidence by each.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
