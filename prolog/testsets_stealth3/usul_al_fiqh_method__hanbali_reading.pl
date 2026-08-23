% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Textualist Source Hierarchy (Usul al-Fiqh)
 *   domain: religious/legal/comparative-law
 *
 * SUMMARY:
 *   Within the contested kernel of Islamic legal methodology (usul al-fiqh),
 *   this story instantiates the Hanbali reading: legal derivation is bounded
 *   maximally by transmitted text (Quran and authenticated hadith), analogy
 *   (qiyas) is admitted only under clear textual silence, a weakly
 *   authenticated report is preferred over human analogy, and suspected
 *   avenues to religious innovation are preventively blocked (sadd
 *   al-dhara'i). The arrangement genuinely coordinates - it solves the
 *   post-prophetic authority problem by giving dispersed communities a
 *   checkable evidentiary standard - while it asymmetrically burdens
 *   rationalist jurists, customary legal development, and devotional
 *   communities whose practices fail the textual vet. Per the
 *   epsilon-invariance principle, the colloquial label 'Islamic legal method'
 *   decomposes into four structurally distinct arrangements; sibling stories
 *   (hanafi_reading, maliki_reading, shafii_reading) instantiate the others,
 *   and this file authors only the Hanbali reading with a single stable
 *   epsilon over its own standing arrangement. Claim and metrics are
 *   independent facts: the type is claimed as tangled_rope from the
 *   structural data (real coordination plus asymmetric, actively enforced
 *   extraction), while the metric values describe the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - - hanbali_school_institution: Agenda-setter (institutional/identity_locked) - administers the textualist method, adjudicates innovation claims, collects interpretive precedence
 *   - - hadith_specialists: Primary beneficiary (organized/mobile) - chain-criticism expertise is the regime's prized currency
 *   - - sharia_observing_laity: Coordinated beneficiary with payer costs (powerless/identity_locked) - receives textual certainty, surrenders legal adaptability
 *   - - rationalist_jurists: Primary target (organized/constrained) - analogy and reasoned-preference toolkits demoted to last resort
 *   - - sufi_practice_communities: Target (moderate/identity_locked) - devotional practices blocked as suspected additions
 *   - - customary_legal_developers: Target (moderate/constrained) - custom admitted only after surviving textual review
 *   - - mu_tazilite_rationalist_theologians: Excluded voice (organized/trapped) - rationalist method marginalized to cautionary status
 *   - - comparative_fiqh_scholars: Analytical observer (analytical/analytical) - maps the four readings' structural differences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.8).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textualist Source Hierarchy (Usul al-Fiqh)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal/comparative-law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'd9076fd5-747d-48d6-8eab-a99f17136b36').
narrative_ontology:cs_kernel_codification('d9076fd5-747d-48d6-8eab-a99f17136b36', fixed_text).
narrative_ontology:cs_authority_grounding('d9076fd5-747d-48d6-8eab-a99f17136b36', lineage).
narrative_ontology:cs_interpretation_layer_present('d9076fd5-747d-48d6-8eab-a99f17136b36').
narrative_ontology:cs_reading_relation('d9076fd5-747d-48d6-8eab-a99f17136b36', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9076fd5-747d-48d6-8eab-a99f17136b36', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9076fd5-747d-48d6-8eab-a99f17136b36', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('d9076fd5-747d-48d6-8eab-a99f17136b36', foundational, transmitted_text_bounds_derivation).
narrative_ontology:cs_axiom_status(transmitted_text_bounds_derivation, holdable).
narrative_ontology:cs_axiom_grounding('d9076fd5-747d-48d6-8eab-a99f17136b36', transmitted_text_bounds_derivation, theological).
narrative_ontology:cs_axiom('d9076fd5-747d-48d6-8eab-a99f17136b36', foundational, weak_report_preferred_over_analogy).
narrative_ontology:cs_axiom_status(weak_report_preferred_over_analogy, holdable).
narrative_ontology:cs_axiom_grounding('d9076fd5-747d-48d6-8eab-a99f17136b36', weak_report_preferred_over_analogy, instrumental).
narrative_ontology:cs_axiom('d9076fd5-747d-48d6-8eab-a99f17136b36', secondary, suspected_novelty_blocked_preventively).
narrative_ontology:cs_axiom_status(suspected_novelty_blocked_preventively, holdable).
narrative_ontology:cs_axiom_grounding('d9076fd5-747d-48d6-8eab-a99f17136b36', suspected_novelty_blocked_preventively, instrumental).
narrative_ontology:cs_reference_frame('d9076fd5-747d-48d6-8eab-a99f17136b36', salaf_textual_norm).
narrative_ontology:cs_drift_state('d9076fd5-747d-48d6-8eab-a99f17136b36', contemporary_state_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d9076fd5-747d-48d6-8eab-a99f17136b36', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hadith_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_school_institution).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, sharia_observing_laity).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_legal_developers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, sufi_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, sharia_observing_laity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, nass_primacy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, salaf_transmission_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the textualist method: trains and credentials jurists, maintains the ranked list of permissible sources, adjudicates accusations of religious innovation, and issues corrective rulings when practice departs from text. Its distinctiveness among the four schools consists precisely in the strictness of its source hierarchy; loosening the hierarchy would erase the boundary that defines it. It bears the ongoing cost of defending the standard against challenge, and it collects precedence, deference, and institutional continuity for maintaining it.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_school_institution, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Transmitters and critics of prophetic reports whose core skill - grading chains of narration - is the regime's prized currency. Under this method their judgments outrank the dialectician's reasoning at nearly every decision point. Their expertise travels: Shafi'i-aligned institutions also reward it, so departure is possible, but the deepest honors and adjudicative seats sit inside the textualist school.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hadith_specialists, beneficiary,
    organized, generational, mobile, global).

% Ordinary worshippers and households who receive rulings certified as anchored in revealed text, which supplies certainty and reassurance that worship follows the Prophet rather than juristic invention. They bear the arrangement's costs indirectly - slower adaptation of law to new commerce, medicine, and finance - and directly whenever a cherished local practice is ruled an impermissible addition and must be abandoned.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, sharia_observing_laity, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, sharia_observing_laity, payer).

% Jurists formed in analogy, reasoned preference, and public-interest deliberation. Inside Hanbali-dominant institutions their tools are demoted to last resorts admitted only under clear textual silence, and reliance on them draws censure. Leaving means shifting affiliation to Hanafi or Maliki institutions - possible, but at the price of standing, position, and years of formation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, regional).

% Devotional communities whose gatherings, litanies, shrine visitation, and festival observances fall under the preventive blocking of suspected additions. Because the practices constitute the spiritual path itself, abandoning them to satisfy the standard would mean abandoning the path; remaining exposes them to periodic campaigns of censure, demolition, and forced simplification.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, sufi_practice_communities, payer,
    moderate, biographical, identity_locked, global).

% Merchants, guilds, and local judges whose working law grows from inherited custom adapted to circumstance. The method admits custom only after it survives textual vetting, so customary commercial and agrarian adaptations stall or proceed informally outside recognized channels; formal recognition requires surrendering the custom's local logic to textual review.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_legal_developers, payer,
    moderate, biographical, constrained, regional).

% The rationalist theological school whose reasoned method lost the post-mihna settlement to textualism. Its heirs would argue that disciplined reason holds a legitimate place in deriving law and doctrine, but they enter the conversation only as historical cautionary figures; within textualist spheres their instruments are cited chiefly as the danger the method exists to prevent.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, mu_tazilite_rationalist_theologians, excluded,
    organized, generational, trapped, regional).

% Modern academic observers who map the four schools' source hierarchies side by side, trace how each reading prices certainty against adaptability, and document enforcement escalations. They hold no stake in the method's persistence and can adopt any analytic frame.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_fiqh_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_school_institution).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes legal derivation around authenticated textual sources: it solves the post-prophetic authority problem - who may speak for revelation - by tying every ruling to the Quran or soundly transmitted hadith, giving dispersed communities a common, checkable evidentiary standard and bounding juristic discretion.
% TRANSFER_FUNCTION: Moves interpretive authority and juridical legitimacy from reasoning-centered jurists (analogy, preference, public interest) to textual specialists and the transmitting class; moves adjudicative discretion from individual jurists to the textual corpus; moves status and institutional precedence to the school that administers the standard.
% ABSENT_VOICES: Rationalist theologians (Mu'tazila) and defenders of customary law are structurally absent: their objections were met by condemning their instruments rather than seating them. Sufi practitioners appear only as objects of innovation adjudication, never as co-authors of the method. Non-Hanbali jurists in mixed jurisdictions are heard only through polemic.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist regime vanished overnight, legal derivation in Hanbali-sphere institutions would reorganize around analogical and customary tools within a generation; innovation policing would relax; devotional practices currently suppressed would re-emerge; the hadith-specialist class would lose its privileged adjudicative position; and the sibling madhhabs would absorb the vacated institutional space.
% FOUNDING_PROBLEM: After the Prophet's death, and acutely during the mihna (the state campaign compelling doctrinal conformity), how can a community derive legitimate law and doctrine without either submitting to state-imposed innovation or licensing unlimited juristic invention? The Hanbali answer: bind derivation to transmitted text.
% FOUNDING_PROBLEM_CORROBORATION: Attestation exists outside the benefiting parties: non-Hanbali chroniclers (al-Tabari, al-Dhahabi) independently record the mihna persecution that forged the textualist stance; comparative-law historiography documents the restrictiveness pattern and its enforcement escalation; and Sufi and reformist literatures from outside the beneficiary set attest the lived costs of innovation policing. Adherents attest the problem remains live; critics inside and outside the tradition attest it is substantially solved and the apparatus now serves boundary-keeping.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 reflects the reading's own accounting of its standing arrangement: the coordination is real and broadly valued, but the costs are asymmetric and concentrated - rationalist toolkits demoted, customary development stalled, devotional practice policed - and they accumulated as enforcement capacity grew. Suppression 0.80 is plausibly the highest among the sibling readings by construction: the method forecloses analogy-led, custom-led, and interest-led alternatives inside its jurisdiction, and the eighteenth-century alliance with state power converted scholarly preference into enforceable orthodoxy. Theater 0.26: derivation remains functional, but a growing share of activity is ritualized condemnation of innovation performed for boundary-keeping rather than case-solving. Accessibility collapse 0.62: once a jurist is formed inside the framework, alternatives largely close, though the sibling madhhabs persist as external exits, keeping the figure below natural-law levels. Resistance 0.58: sustained pushback from rationalists, Sufi communities, and reformers, and from within - Ibn Taymiyya's own expansive use of analogy shows the restriction is a policy, not a physical limit. The temporal series shares one grid (t=0 marks the school's formative consolidation in the late ninth century CE; t=30 the contemporary era); the suppression_requirement rise models enforcement-capacity buildup (formative defensiveness, classical consolidation, revival-era state enforcement), not merely shifting extraction. Suppression is authored as a raw structural property; only the engine scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the administering institution's seat the arrangement is the coordination it built and staffs - a faithful guard over revelation. From the rationalist jurist's seat the same hierarchy operates as enforced dispossession of its craft. From the laity's seat it splits: certainty received, adaptability surrendered. From the Sufi seat it is a standing threat to practices constitutive of identity. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: the school institution sits nearest the beneficiary end (it sets the standard and collects from it), and hadith specialists carry low effective burden because their skills hold arbitrage-grade mobility toward Shafi'i institutions. The laity's dual declaration (beneficiary collecting certainty, paying in adaptability) should land them near symmetric. Victim declarations drive high directionalities: rationalist jurists and customary developers are constrained (exit exists but is costly in standing and formation), and Sufi communities are identity-locked - their practices are the thing itself - placing them nearest the full-target end. Receipt: the arrangement's gains - interpretive monopoly, adjudicative precedence, institutional continuity - demonstrably accrue to the administering institution, which is why gain_flow names that seat rather than diffuse. Fixing: the seat that could relax the standard is the institution itself, and relaxation would cost it the boundary that constitutes it, so the cost class is prohibitive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - protecting revealed law against imposed and invented innovation, forged in the mihna's coercion - is genuinely contested rather than dead: adherents hold that novelty requiring policing never ceases, while critics inside and outside the tradition hold the protective problem substantially solved and the apparatus maintained for boundary-keeping and institutional advantage. The classification prevents two mislabels: reading the arrangement as pure extraction would erase the real coordination (a checkable textual standard, bounded juristic discretion) that laity and hadith specialists demonstrably value; reading it as pure coordination would erase the asymmetric, escalating burdens the victim seats bear. Contested founding status combined with a load-bearing disappearance verdict (world_rearranges) flags - without resolving - the possibility that enforcement now serves identity maintenance beyond the founding purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    usul_kernel_reading_contest,
    'This story instantiates the hanbali_reading of the usul_al_fiqh_method kernel; would the hanafi, maliki, or shafii readings of the same kernel yield materially different epsilon and victim structures over their own arrangements?',
    'Generate the three sibling stories under identical authoring rules and compare epsilon, suppression, and victim sets across the family.',
    'Wide divergence confirms the colloquial label conflates structurally distinct arrangements (already handled by decomposition); convergence would suggest the readings differ more in rhetoric than structure, weakening this reading''s claimed distinctiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(usul_kernel_reading_contest, conceptual, 'Whether the four madhhab readings of the usul al-fiqh kernel are structurally distinct constraints.').

omega_variable(
    weak_hadith_accuracy_tradeoff,
    'Does preferring a weakly authenticated report over disciplined analogy actually produce less reliable rulings than the alternatives it excludes?',
    'Comparative isnad-criticism studies scoring rulings sourced to weak chains against verified outcomes, benchmarked against analogical derivations on matched cases.',
    'If weak-chain rulings err more often, the textual-fidelity framing conceals an accuracy cost borne by ruled populations and epsilon is understated; if not, the preference is vindicated as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_accuracy_tradeoff, empirical, 'Accuracy cost of the weak-hadith-over-analogy preference.').

omega_variable(
    sadd_scope_novelty_vs_harm,
    'Is the preventive blocking of innovations exercised mainly against demonstrably harmful avenues, or mainly against mere novelty?',
    'Audit blocked practices across periods: classify each block as harm-evidenced or novelty-only, and track the ratio over time.',
    'A novelty-dominated ratio raises true suppression above the authored value and pushes the arrangement toward the extraction-dominant end; a harm-dominated ratio supports the preventive-coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_scope_novelty_vs_harm, conceptual, 'Whether sadd al-dhara''i functions as harm prevention or novelty veto.').

omega_variable(
    bida_definition_drift,
    'Which definition of religious innovation binds - the tolerant classical range (some additions permitted) or the narrow revival-era range (nearly all additions condemned)? The victim set depends on which governs.',
    'Corpus analysis of fatwa and verdict collections across periods, measuring the migration of the tolerated-practice boundary.',
    'Narrowing definitions enlarge the victim set and raise effective extraction; the authored metrics assume the revival-era narrowing is already operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bida_definition_drift, conceptual, 'Contested definition of bid''a drives victim-set size.').

omega_variable(
    state_alliance_enforcement_attribution,
    'How much of the measured enforcement intensification after the eighteenth century is attributable to the state alliance rather than endogenous scholarly dynamics?',
    'Compare enforcement patterns in Hanbali communities under and outside state patronage across the same period.',
    'State attribution locates the suppression spike in political coupling and suggests decoupling remedies; endogenous attribution implicates the method itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_alliance_enforcement_attribution, empirical, 'Attribution of the enforcement ratchet to the state alliance.').

omega_variable(
    cs_framing_lineage_vs_practice,
    'The commitment-system framing declares lineage authority (chains of transmission ground legitimacy); an equally coherent framing declares practice authority (the school''s operative usage is itself the standard). Does the alternative framing change the computed drift or foreclosure profile?',
    'Re-run classification under the practice-grounded framing and compare drift direction and axiom-contradiction outcomes.',
    'Under practice grounding, the contemporary gap reads as ordinary practice evolution rather than axiom overriding, softening the drift vector; the lineage framing was chosen because the tradition''s own legitimacy claims run through transmission chains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_lineage_vs_practice, conceptual, 'Framing under-determination in the commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t5, usul_al_fiqh_method__hanbali_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(usul_tr_t5, observed).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanbali_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(usul_tr_t10, observed).
narrative_ontology:measurement(usul_tr_t15, usul_al_fiqh_method__hanbali_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(usul_tr_t15, observed).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanbali_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(usul_tr_t20, observed).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__hanbali_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(usul_tr_t25, observed).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanbali_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(usul_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t5, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(usul_be_t5, observed).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(usul_be_t10, observed).
narrative_ontology:measurement(usul_be_t15, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(usul_be_t15, observed).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(usul_be_t20, observed).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(usul_be_t25, observed).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(usul_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t5, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(usul_su_t5, observed).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(usul_su_t10, observed).
narrative_ontology:measurement(usul_su_t15, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(usul_su_t15, observed).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(usul_su_t20, observed).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(usul_su_t25, observed).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement_basis(usul_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal methodology' decomposes, per the epsilon-invariance principle, into four structurally distinct arrangements - one per madhhab reading of the usul_al_fiqh_method kernel. Each sibling gets its own epsilon, beneficiary/victim structure, and classification; this story authors the Hanbali reading only. The upstream/downstream structure runs through shared textual commitments: the Shafi'i authentication discipline and the Hanafi analogy tradition are the two poles against which the Hanbali reading defines its distinctive position (restrictive scope for analogy, tolerance for weak reports), so edges are declared to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
