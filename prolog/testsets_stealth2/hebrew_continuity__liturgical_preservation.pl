% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Liturgical Preservation Regime of Hebrew Continuity
 *   domain: sociolinguistics/religious-commitment-systems
 *
 * SUMMARY:
 *   After Hebrew ceased to be anyone's daily vernacular (by roughly the close
 *   of the Talmudic era, mapped here to interval t=0), the community kept it
 *   alive — on this reading's definition of 'alive' — through daily
 *   liturgical recitation, the public reading cycle, scribal exactitude, and
 *   compulsory childhood instruction, administered by a rabbinic class whose
 *   standing rests on being the transmission chain. The arrangement solves a
 *   real problem: a territorially dispersed population retains direct access
 *   to one shared corpus and one portable liturgy with no native-speaker base
 *   at all. It simultaneously concentrates interpretive authority in the
 *   administering class, imposes years of non-consented labor on the young,
 *   and repeatedly restricted alternatives (vernacular prayer,
 *   translation-first access, private devotional substitutes). Authored as
 *   one reading of a contested kernel; see kernel_context for the committer
 *   note. KEY AGENTS (by structural relationship): -
 *   rabbinic_interpretive_elite: agenda-setting seat
 *   (institutional/identity_locked) — administers curriculum, recitation
 *   standards, textual accuracy - professional_text_transmitters: beneficiary
 *   seat (organized/constrained) — scribes, readers, teachers paid from
 *   communal funds - diaspora_jewish_communities: beneficiary seat with cost
 *   side (organized/constrained) — receive portable liturgy, fund the
 *   apparatus - hebrew_schoolchildren: primary target seat
 *   (powerless/trapped) — bear the recitation and decoding labor before any
 *   capacity to consent - vernacular_liturgists: target seat with exit
 *   (moderate/mobile) — reform advocates answered with bans; left by schism -
 *   assimilation_inclined_members: target seat (moderate/constrained) — pay
 *   individually under social sanction - women_barred_from_advanced_study:
 *   excluded seat (powerless/trapped) — obligations without access, no seat
 *   in setting the rules - sociolinguistic_researchers: analytical observer
 *   (analytical/analytical) — compares the regime with Latin, Qur'anic
 *   Arabic, Sanskrit parallels
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_elite: agenda-setting seat (institutional/identity_locked) — sets what is recited, how texts are copied and taught; authority flows from indispensability to the transmission chain
 *   - professional_text_transmitters: beneficiary seat (organized/constrained) — scribes, public readers, and teachers compensated from communal funds for the preservation function
 *   - diaspora_jewish_communities: beneficiary seat with a cost side (organized/constrained) — receive identical liturgy and corpus access worldwide; levy and pay the funding that sustains schools and scribes
 *   - hebrew_schoolchildren: primary target seat (powerless/trapped) — years of daily recitation and letter-by-letter decoding before any capacity to consent or exit
 *   - vernacular_liturgists: target seat with exit (moderate/mobile) — advocates of worship in the surrounding language; met prohibitions and excommunication; exited by founding parallel institutions
 *   - assimilation_inclined_members: target seat (moderate/constrained) — members pulled toward surrounding vernacular culture; sanction prices their drift individually
 *   - women_barred_from_advanced_study: excluded seat (powerless/trapped) — carried the tradition's obligations while formal text education was reserved for boys
 *   - sociolinguistic_researchers: analytical observer (analytical/analytical) — comparative seat across postvernacular liturgical-language regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.52).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Liturgical Preservation Regime of Hebrew Continuity").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/religious-commitment-systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '7440ed62-90cd-4236-b408-3909ea258ca5').
narrative_ontology:cs_kernel_codification('7440ed62-90cd-4236-b408-3909ea258ca5', fixed_text).
narrative_ontology:cs_authority_grounding('7440ed62-90cd-4236-b408-3909ea258ca5', lineage).
narrative_ontology:cs_interpretation_layer_present('7440ed62-90cd-4236-b408-3909ea258ca5').
narrative_ontology:cs_reading_relation('7440ed62-90cd-4236-b408-3909ea258ca5', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('7440ed62-90cd-4236-b408-3909ea258ca5', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('7440ed62-90cd-4236-b408-3909ea258ca5', foundational, sacred_use_constitutes_language_life).
narrative_ontology:cs_axiom_status(sacred_use_constitutes_language_life, holdable).
narrative_ontology:cs_axiom_grounding('7440ed62-90cd-4236-b408-3909ea258ca5', sacred_use_constitutes_language_life, theological).
narrative_ontology:cs_axiom('7440ed62-90cd-4236-b408-3909ea258ca5', secondary, textual_engagement_obligation).
narrative_ontology:cs_axiom_status(textual_engagement_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7440ed62-90cd-4236-b408-3909ea258ca5', textual_engagement_obligation, deontological).
narrative_ontology:cs_reference_frame('7440ed62-90cd-4236-b408-3909ea258ca5', mesorah_unbroken_transmission_chain).
narrative_ontology:cs_drift_state('7440ed62-90cd-4236-b408-3909ea258ca5', contemporary_post_emancipation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7440ed62-90cd-4236-b408-3909ea258ca5', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, professional_text_transmitters).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, hebrew_schoolchildren).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, vernacular_liturgists).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, assimilation_inclined_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets what is recited, how it is pronounced, which texts are authoritative, and how children are taught; adjudicates deviation up to excommunication. Standing, livelihood, and marriage prospects within the community flow from being the indispensable link in the transmission chain; leaving the role would mean surrendering the position that decades of training built. The role and the person have grown into one thing.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite, beneficiary).

% Scribes, public readers, and teachers employed from communal funds precisely to copy, chant, and instruct. Their skills carry little market value outside the tradition, so their livelihood is bound to the institution's health; they collect steady compensation and honored standing for work they could not otherwise sell.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, professional_text_transmitters, beneficiary,
    organized, biographical, constrained, continental).

% Receive a liturgy that works identically in distant cities and direct access to a shared corpus without translation intermediaries. They also levy and pay the communal funds that sustain schools, synagogues, scribes, and printing, and they apply discipline to members who drift toward surrounding languages. Leaving means assimilating out of the community entirely, which most members experience as losing the family's place rather than changing languages.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, payer).

% Sit through years of daily recitation and letter-by-letter decoding, much of it before understanding what the sounds mean; the payoff, if it comes, arrives in adulthood as access and standing. They cannot decline, bargain over the curriculum, or leave; their consent is structurally unavailable, and the hours spent are unrecoverable either way.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, hebrew_schoolchildren, payer,
    powerless, biographical, trapped, regional).

% Prayed and argued for services in the surrounding language, with preaching people could follow. The authorities answered with prohibitions on vernacular prayer books, public denunciations, and in sharp cases excommunication and destruction of offending editions. Their way out was schism: a costly severance of family and community ties, but a real door, and many walked through it to found parallel institutions.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, vernacular_liturgists, payer,
    moderate, biographical, mobile, continental).

% Members drawn to the surrounding culture's language and manners, for whom the Hebrew obligations register as a toll on belonging. Social sanction, matchmaking consequences, and family pressure raise the price of drifting; unlike the reformers they rarely organize, so they pay individually and quietly, and most stay.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, assimilation_inclined_members, payer,
    moderate, biographical, constrained, national).

% Carried the tradition's household and ritual obligations while formal text education was reserved for boys; most learned prayers by heart without being taught to read them. Had they been seated when curricula and eligibility rules were set, the pedagogy and the access rules would have been contested from inside; the objection existed but had no address.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, women_barred_from_advanced_study, excluded,
    powerless, biographical, trapped, regional).

% Study the regime alongside Church Latin, Qur'anic Arabic, and liturgical Sanskrit: which mechanisms kept a postvernacular language functional, where comprehension separated from recitation, and what happened when external enforcement stopped. They collect and compare; nothing flows to or from them under the arrangement.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, sociolinguistic_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a territorially dispersed population tied to one sacred corpus and one liturgy with no native-speaker base: standardized recitation lets any member participate in any congregation worldwide, and scribal plus pedagogical discipline keeps the texts directly readable across generations.
% TRANSFER_FUNCTION: Moves childhood time and labor (years of recitation and text-decoding), communal funds (schools, synagogues, scribes, printing), and interpretive authority upward to the administering and transmitting classes; moves symbolic continuity, corpus access, and membership standing outward to the whole community.
% ABSENT_VOICES: Women long barred from formal text education carried obligations without corresponding access and had no seat in setting pedagogy; rank-and-file reciters who did not understand what they pronounced had no channel to question method; vernacular-minded reformers were answered with bans and excommunication rather than seating — several ultimately left by schism, which is where their objection went.
% DISAPPEARANCE_RATIONALE: Overnight removal would not restore native speech — that capacity was already gone — but the diaspora's linguistic bond would unravel within a few generations: liturgies would vernacularize locally and diverge region by region, direct textual access would collapse into dependence on translation chains, the transmitter professions would vanish, and the administering class would lose the authority base the transmission chain provides. Community boundaries themselves would blur faster, since the shared language is a principal membership marker.
% FOUNDING_PROBLEM: After Hebrew stopped being spoken (roughly by the close of the Talmudic era), a dispersed community needed its sacred texts kept readable and its liturgy keepable by people who would never acquire the language natively — without letting either drift into mutual unintelligibility.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: sociolinguists of religion (Joshua Fishman's studies of postvernacular languages treat this regime as the paradigm case) and the documented parallel trajectories of Church Latin, Qur'anic Arabic, and liturgical Sanskrit attest that the non-native-access problem is real and recurring; internally, recurring complaints about the schooling burden from the parents who paid it — and the reformers' schisms — attest the costs were felt by those who bore them. The administering class's own testimony that the problem persists is self-interested and weighted accordingly.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the regime transfers real, unrecoverable resources — childhood hours, communal funds, linguistic autonomy — while returning genuine access and continuity; the transfer is substantial but not confiscatory, and part of the cost purchases a coordination good the payers themselves consume. Suppression 0.62: persistence depends on active machinery (compulsory schooling, communal discipline, prohibition of vernacular liturgy, excommunication in sharp cases), not on spontaneous preference; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope — the engine owns any scaling of extractiveness only. Theater_ratio 0.24: within this reading recitation IS the function, so performance and function overlap heavily; the ratio tracks the growing share of sound-without-comprehension activity as vernacular comprehension receded, rising from near-zero in the early scholarly era to roughly a quarter of activity at interval end. Accessibility_collapse 0.5: alternatives (translated liturgy, private devotion, exit) exist but each carries the price of schism or assimilation out of the community, so they narrow without vanishing. Resistance 0.6: this regime met sustained organized resistance — the Karaite schism, the maskilic critique of rote schooling, the reform movement's vernacular campaigns — which is why suppression sits above extraction in the profile. The measurement series runs on ONE shared nine-point grid (century-scale points 0–1600, mapping approximately 400 CE to 2000 CE) so every tracked metric is authored at every examined time point; suppression_requirement is authored because enforcement capacity materially changed over the interval (gaonic consolidation, medieval communal ordinances, emancipation-era hardening peaking around t=1400, then partial stabilization), not merely shifted extraction. Coordination type identity_coordination: the failure test picks the dominant function — if recitation and transmission lapsed, the first casualty is the community's capacity to maintain membership coherence and corpus-bound identity, ahead of any informational loss; the type default floor stands, no override.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the administering seat the arrangement is vocation and continuity: the same structure that costs the child his mornings is the rabbi's entire standing, and its defense looks like guardianship. From the child's seat it is compelled labor with a deferred, uncertain payoff and no exit at any age. From the community seat it is simultaneously insurance (a liturgy that works in any city, direct access to the corpus) and a tax (levies, schooling hours, disciplinary exposure). Same-level divergence: vernacular_liturgists and assimilation_inclined_members hold similar power and both pay, but the reformers could organize and walk — schism was a real door — while the assimilation-inclined pay alone under family and matchmaking pressure, so equal standing produces unequal effective pressure purely through exit structure. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: the administering elite lowest of all (agenda-setter plus collector, identity-locked into the role); transmitters low but damped by the fact that their gain is wage-compensation for service rather than rent; communities low-to-moderate because they also carry the funding and disciplinary cost side (secondary payer). Targets sit near the full-target end with exit-modulated spread: schoolchildren highest (powerless, trapped, no consent possible at any age); vernacular liturgists high but damped by genuine mobility through schism; assimilation-inclined members high with weaker damping, since their exit is socially priced rather than organizationally available. The excluded women's seat feeds the absent-voice record, not the derivation. On scope: the regime operates globally, which makes 'is the language actually alive' expensive to verify and amplifies effective extraction at the target seats accordingly — the engine owns that arithmetic; this file supplies the scope atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification guards both symmetrical errors. Calling the regime pure extraction ignores a sixteen-century coordination achievement: a population with zero native speakers kept direct textual access and a unified liturgy across three continents — the coordination function is genuine and load-bearing, which bars the snare verdict. Calling it pure coordination ignores the concentrated interpretive authority, the non-consented childhood labor, and the prohibited alternatives — which bars the rope verdict. Mandatrophy is NOT declared: within this reading the founding problem (a non-native population needing sanctioned access to its texts) is still live wherever the reading governs, so founding_problem_status=live combined with disappearance_verdict=world_rearranges yields no zombie flag under the mismatch consumer. The live contingency — that the sibling native_generative reading could win the kernel contest and kill this arrangement's mandate retroactively — is routed to the omega variables, where committer structure belongs, not into this file's flags.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the liturgical_preservation reading of the hebrew_continuity kernel; which of the three readings'' vitality criteria should govern classification when they diverge?',
    'Comparative classification across the three sibling stories (hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized); divergence in computed type or epsilon across siblings marks the criterion dispute as substantive rather than definitional.',
    'Adopting the native_generative criterion would re-point epsilon at the preservation regime as the arrangement that blocks speaker formation (its subjects become the cost-bearers qua non-speakers); adopting the bridge_pidginized criterion would re-point epsilon at contact-medium adequacy. This file''s epsilon (0.52) is valid only under the liturgical_preservation criterion over the standing preservation regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer routing: this constraint is one reading of a three-reading kernel; sibling readings are separate files.').

omega_variable(
    vitality_predicate_location,
    'Where exactly do the readings disagree: on the predicate ''lives'' (what counts as a language being alive), on the necessary conditions (native speakers vs transmission vs communicative function), or on the referent (which Hebrew is under evaluation)?',
    'Structural comparison of the sibling axiom sets: if the disagreement reduces to the necessity claim about native speakers, foreclosure analysis between this reading and native_generative applies; if it reduces to the predicate itself, the readings are incommensurable without a shared metric and the kernel stays contested indefinitely.',
    'Determines whether the kernel can be adjudicated empirically (speaker-count and usage trajectories) or only conceptually; the second outcome entrenches all three readings as permanent parallel constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_predicate_location, conceptual, 'Locates the structural element on which the sibling readings actually diverge.').

omega_variable(
    comprehension_recitation_gap,
    'Does recitation-without-comprehension progressively hollow the transmission function (rising performative share), or does symbolic competence sustain direct corpus access well enough to keep the coordination load-bearing?',
    'Longitudinal comprehension-versus-recitation testing in communities that continue the practice, plus philological evidence of interpretive depth per era; the measurement series in this file models the trend the tests would confirm or break.',
    'Sustained theater-ratio growth would push the arrangement toward inertial, performance-maintained operation within this reading''s own terms; stable comprehension supports the genuine-coordination half of the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comprehension_recitation_gap, empirical, 'Whether symbolic preservation degrades into pure performance over time.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the enforcement that holds the regime together primarily structural (compulsory schooling, communal sanction, access control) or internalized (the duty experienced as self-evident obligation requiring no external machinery)?',
    'Post-emancipation natural experiment: compare communities where legal and communal coercive capacity was removed — did practice persist at prior intensity (internalized) or decay with the machinery (structural)?',
    'If largely internalized, the measured suppression understates the regime''s persistence capacity and it survives enforcement collapse; if structural, the regime tracks enforcement budgets and weakens as they do.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized enforcement mechanism.').

omega_variable(
    victim_set_framing,
    'This reading frames secularizing currents as the threat to the textual tradition; are the operative cost-bearers the framed threat''s carriers (reformers, assimilation-inclined members) or the regime''s own subjects (schoolchildren, reciters who do not comprehend what they pronounce)?',
    'Trace sanction and opportunity-cost incidence directly: who actually bears penalties, lost hours, and restricted options, as opposed to who is rhetorically named as the danger.',
    'Re-weighting the victim set toward the regime''s own subjects would widen the directionality spread and could shift the computed classification toward the purely extractive end; keeping the framed-threat weighting preserves the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_framing, conceptual, 'Framed threat versus actual cost-bearer asymmetry inside this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t200, hebrew_continuity__liturgical_preservation, theater_ratio, 200, 0.09).
narrative_ontology:measurement(hebr_tr_t400, hebrew_continuity__liturgical_preservation, theater_ratio, 400, 0.1).
narrative_ontology:measurement(hebr_tr_t600, hebrew_continuity__liturgical_preservation, theater_ratio, 600, 0.11).
narrative_ontology:measurement(hebr_tr_t800, hebrew_continuity__liturgical_preservation, theater_ratio, 800, 0.13).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_continuity__liturgical_preservation, theater_ratio, 1000, 0.14).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_continuity__liturgical_preservation, theater_ratio, 1200, 0.17).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_continuity__liturgical_preservation, theater_ratio, 1400, 0.21).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_continuity__liturgical_preservation, theater_ratio, 1600, 0.24).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_be_t200, hebrew_continuity__liturgical_preservation, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(hebr_be_t400, hebrew_continuity__liturgical_preservation, base_extractiveness, 400, 0.42).
narrative_ontology:measurement(hebr_be_t600, hebrew_continuity__liturgical_preservation, base_extractiveness, 600, 0.44).
narrative_ontology:measurement(hebr_be_t800, hebrew_continuity__liturgical_preservation, base_extractiveness, 800, 0.46).
narrative_ontology:measurement(hebr_be_t1000, hebrew_continuity__liturgical_preservation, base_extractiveness, 1000, 0.47).
narrative_ontology:measurement(hebr_be_t1200, hebrew_continuity__liturgical_preservation, base_extractiveness, 1200, 0.49).
narrative_ontology:measurement(hebr_be_t1400, hebrew_continuity__liturgical_preservation, base_extractiveness, 1400, 0.53).
narrative_ontology:measurement(hebr_be_t1600, hebrew_continuity__liturgical_preservation, base_extractiveness, 1600, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebr_su_t200, hebrew_continuity__liturgical_preservation, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(hebr_su_t400, hebrew_continuity__liturgical_preservation, suppression_requirement, 400, 0.46).
narrative_ontology:measurement(hebr_su_t600, hebrew_continuity__liturgical_preservation, suppression_requirement, 600, 0.5).
narrative_ontology:measurement(hebr_su_t800, hebrew_continuity__liturgical_preservation, suppression_requirement, 800, 0.54).
narrative_ontology:measurement(hebr_su_t1000, hebrew_continuity__liturgical_preservation, suppression_requirement, 1000, 0.56).
narrative_ontology:measurement(hebr_su_t1200, hebrew_continuity__liturgical_preservation, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(hebr_su_t1400, hebrew_continuity__liturgical_preservation, suppression_requirement, 1400, 0.66).
narrative_ontology:measurement(hebr_su_t1600, hebrew_continuity__liturgical_preservation, suppression_requirement, 1600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% 'Hebrew lives' is a colloquial label covering structurally distinct claims; per the epsilon-invariance principle it decomposes into three stories sharing the hebrew_continuity kernel. This story carries the liturgical_preservation branch: its epsilon (0.52) measures the preservation regime's transfer of labor, funds, and linguistic autonomy against its coordination yield. The native_generative sibling measures a different arrangement (speaker-forming versus preserving institutions) with a different cost-bearing set; the bridge_pidginized sibling measures contact-medium adequacy. Edges run from this story to both siblings because the preservation regime historically supplied the textual substrate and standardized forms the other readings presuppose — the upstream claim is cited as evidence downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
