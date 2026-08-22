% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Constraint Authority — Feudal Obsolescence Reading
 *   domain: constitutional history/legal philosophy/political theory
 *
 * SUMMARY:
 *   The standing arrangement under contest: Magna Carta is retained as a
 *   residual statute and a premier national symbol while stripped of
 *   operative force. Of the 1297 confirmation, revision statutes have
 *   repealed everything except a handful of clauses; no minister can be sued
 *   on the charter, no policy falls for violating it, and courts cite it as
 *   historical background while deciding on modern grounds. Meanwhile the
 *   document is enshrined, exhibited, commemorated on anniversaries, and
 *   invoked by ministers as the fountainhead of the rule of law. This story
 *   instantiates the feudal_obsolescence_reading of the kernel
 *   magna_carta_constraint_authority: the claim that the charter was a
 *   baronial compact addressing thirteenth-century feudal grievances and
 *   carries no binding authority over modern sovereignty structures. Assessed
 *   by this reading's own lights, the arrangement is legitimate in its
 *   inertness — and yet the reading's own referent (the ceremonial retention
 *   of a spent instrument whose prestige is continuously harvested) exhibits
 *   the profile of an atrophied restraint: enormous performative maintenance,
 *   near-zero function, diffuse costs borne by those who still take the
 *   promise seriously, and no seat that captures the yield or defends the
 *   shell. Family relationship: this is one of three linked readings; the
 *   living-constitutionalism sibling assigns the charter binding force
 *   through juridical evolution (shrinking this story's victim set to near
 *   zero and bounding executive discretion), and the
 *   parliamentary-sovereignty sibling relocates the charter's force into
 *   enactable, repealable statute. Each sibling file carries its own epsilon,
 *   victims, and type; nothing is averaged here.
 *
 * KEY AGENTS:
 *   - - uk_parliament: agenda-setter (institutional/arbitrage) — administers the residual charter statute; holds unilateral power to repeal the remainder or entrench it, and has done neither
 *   - - crown_and_executive_government: principal beneficiary (institutional/arbitrage) — governs without any charter-based limit while drawing on the charter's prestige
 *   - - judiciary: administering interpreter (institutional/constrained) — decides case by case that charter text is background rather than operative law
 *   - - popular_constitutionalist_movements: primary payer (organized/identity_locked) — movements whose identity presupposes the charter still speaks to present power
 *   - - charter_invoking_litigants: payer (moderate/mobile) — parties who spend advocacy on charter arguments that predictably fail
 *   - - heritage_commemoration_institutions: secondary beneficiary (organized/constrained) — curates and monetizes the charter's fame
 *   - - colonial_subjects_denied_charter_rights: excluded voice (powerless/trapped) — historical populations refused the charter's protections on territorial-limitation reasoning
 *   - - legal_historians_academy: analytical observer (moderate/analytical) — documents the gap between the charter's feudal content and its modern reputation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.28).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.84).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.84).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Constraint Authority — Feudal Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional history/legal philosophy/political theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e0c82be2-33e2-4862-a192-a6aa2e102312').
narrative_ontology:cs_kernel_codification('e0c82be2-33e2-4862-a192-a6aa2e102312', fixed_text).
narrative_ontology:cs_authority_grounding('e0c82be2-33e2-4862-a192-a6aa2e102312', lineage).
narrative_ontology:cs_interpretation_layer_present('e0c82be2-33e2-4862-a192-a6aa2e102312').
narrative_ontology:cs_reading_relation('e0c82be2-33e2-4862-a192-a6aa2e102312', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('e0c82be2-33e2-4862-a192-a6aa2e102312', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e0c82be2-33e2-4862-a192-a6aa2e102312', foundational, charter_normativity_context_bound).
narrative_ontology:cs_axiom_status(charter_normativity_context_bound, holdable).
narrative_ontology:cs_axiom_grounding('e0c82be2-33e2-4862-a192-a6aa2e102312', charter_normativity_context_bound, conventional).
narrative_ontology:cs_axiom('e0c82be2-33e2-4862-a192-a6aa2e102312', secondary, unenacted_charter_text_non_justiciable).
narrative_ontology:cs_axiom_status(unenacted_charter_text_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('e0c82be2-33e2-4862-a192-a6aa2e102312', unenacted_charter_text_non_justiciable, empirically_contingent).
narrative_ontology:cs_reference_frame('e0c82be2-33e2-4862-a192-a6aa2e102312', charter_as_discharged_feudal_compact).
narrative_ontology:cs_drift_state('e0c82be2-33e2-4862-a192-a6aa2e102312', contemporary_commemoration_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('e0c82be2-33e2-4862-a192-a6aa2e102312', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_institutions).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalist_movements).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_invoking_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the statute book containing the 1297 confirmation of the charter. Successive revision acts have repealed every clause except a small residue; it retains full power to repeal the remainder or to entrench it, and has chosen neither, maintaining the text through routine consolidation while lending support to commemorations. Nothing about the arrangement costs parliament anything, and no constituency pressures it to change course.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Governs under prerogative and statute without facing any charter-based limit: no minister can be sued on the charter and no policy falls for violating it. Its spokesmen invoke the charter's prestige at ceremonies and in speeches about the rule of law, and successive governments have resisted litigants' attempts to rely on the charter's text. Exit is meaningless here — the arrangement confers freedom of action that no alternative arrangement would enlarge.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Decides, case by case, whether arguments drawn from the charter affect outcomes. In practice it treats the charter as historical background and resolves cases on statute, common law, and convention, declining to give the 1215 or 1297 text direct operative effect. Individual judges praise the charter's symbolism in extra-judicial writing. The bench is bound by settled practice and by the priority of enacted law; no single court can revive the charter's force unilaterally.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Campaign groups and civic movements that treat the charter as a standing promise by rulers to the ruled — the ancestor of their claims about due process, jury trial, and limits on detention. Their publications, rallies, and educational work presuppose that the charter still speaks to present power. Abandoning that premise would dissolve the movements' founding identity, so they continue organizing around a document that answers none of their litigation.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalist_movements, payer,
    organized, generational, identity_locked, national).

% Parties — detainees, protesters, campaigners — who cite charter clauses in pleadings and submissions, usually alongside modern grounds. Their charter arguments are regularly rejected as historically superseded, and the effort spent on them is lost. Unlike the movements, individual litigants lose little by dropping the argument: they reroute to human-rights legislation and judicial review and continue their cases.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_invoking_litigants, payer,
    moderate, biographical, mobile, national).

% Museums, cathedrals, trusts, and tourism bodies that hold charter manuscripts and run exhibitions, anniversaries, and educational programs built on the document's fame. Their income and public profile depend on continued public reverence for the charter, quite apart from any legal effect. Their assets and programming are tied to the manuscript economy, so they cannot redeploy elsewhere.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_institutions, beneficiary,
    organized, biographical, constrained, national).

% Populations under imperial rule to whom administrators and courts refused the charter's protections, holding that it ran only within the realm or only to the monarch's English subjects. Petitioners in the colonies who invoked it were turned away; the refusal was defended with the same territorial-and-temporal limitation reasoning this reading generalizes. They were never seated in any forum that fixed the charter's reach.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, colonial_subjects_denied_charter_rights, excluded,
    powerless, biographical, trapped, global).

% Scholars of medieval law and constitutional history who study the charter's 1215 context, its reissues, and its reception. Their work documents the distance between the charter's feudal content and its modern reputation. They hold no stake in the myth's survival or demolition and publish from outside every positioned seat.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, legal_historians_academy, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates civic memory: a shared origin narrative of ruler-self-restraint that schools, ceremonies, and political speech use as a common reference point, and a residual statutory presence that remains nominally available to courts. The live coordination performed is thin — chiefly the commemorative calendar and a common vocabulary for rule-of-law talk.
% TRANSFER_FUNCTION: Moves symbolic legitimation and civic trust from the public to the governing order — deference earned by invoking an inherited promise of restraint — and absorbs litigants' and movements' advocacy effort into arguments that fail. Nothing material moves back toward those who supply the deference or the effort.
% ABSENT_VOICES: Colonial petitioners who invoked the charter and were refused on territorial-limitation grounds are the paradigmatic absent voice — historically excluded from every forum that fixed the charter's reach. Today, detainees and defendants who raise charter clauses appear only as losing parties; no seat inside the arrangement represents the view that the promise should be either honored or formally retired.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — residual clauses repealed, commemorations ended, the myth retired — the operating constitution would continue unchanged: judicial review, statute, and elections already do all the restraining. What rearranges is the symbolic economy: the heritage sector loses its anchor artifact's civic role, political speech loses its oldest prestige resource, civic education loses its founding scene, and the popular-constitutionalist movements lose the totem around which they are organized.
% FOUNDING_PROBLEM: King John's capricious rule: scutage and aids levied without consent, punitive reliefs and wardship abuses, disseisin of baronial lands, and justice sold, denied, or delayed. The 1215 settlement was negotiated by rebel barons to secure their tenurial and judicial position against the crown.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by the legislative record itself — the nineteenth-century Statute Law Revision Acts repealed the feudal clauses as spent — and by academic historiography of Angevin finance and tenure, which locates every operative grievance in a tenurial system that no longer exists. No beneficiary of the present arrangement attests that the founding problem is live.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness 0.62: the arrangement's yield is symbolic legitimation harvested by the governing order plus advocacy effort absorbed from litigants — not material tribute, but real and rising, because the myth's reach expanded with mass education and global Anglosphere reverence while the delivered restraint stayed at zero, widening the promise-delivery gap. Suppression 0.28: there is no coercive machinery; the mechanism is dismissal and irrelevance — one may invoke the charter freely and lose — with mild forum-closure where courts refuse charter grounds. Theater_ratio 0.84: commemoration, exhibition, anniversary politics, and rhetorical invocation dominate; functional activity is confined to a few residual clauses that are almost never operative. Accessibility_collapse 0.25: substitutes are abundant and intact (human-rights legislation, judicial review, electoral accountability), so understanding the charter's inertness traps no one. Resistance 0.38: periodic revival attempts — clause 39 in due-process debates, anniversary mobilization, occasional judicial praise — real but marginal. Temporal design: the interval indexes years since 1900 (t = year − 1900, so t=120 is 2020); both tracked series run on one shared seven-point grid, endpoints matching the scalars. The suppression_requirement series is deliberately omitted: the enforcement picture is static (settled dismissal practice, no build-up or decay of enforcement capacity), so the scalar captures it. The theater series rises monotonically with superposed anniversary spikes (notably the 2015 octocentenary near t=115, smoothed into the t=100–120 segment); the spikes are episodic publicity, not an oscillating extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very differently from the same structure. From the executive seat the arrangement is pure subsidy: inherited discretionary space plus a prestige resource, at zero cost. From the popular-constitutionalist seat — identity-locked, since accepting the obsolescence premise dissolves the movement's founding identity — the same arrangement reads as dispossession of an inheritance, and effective extraction is amplified by the lock-in. From the litigant seat the loss is real but shallow: mobile exit onto modern grounds damps the experienced burden. Parliament, the seat that could change everything, bears essentially nothing and therefore registers near-zero salience. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. modern_executive_governments sit near the beneficiary pole (subsidized discretion, arbitrage-grade exit). heritage_commemoration_institutions derive low directionality — they collect adjacent commercial and reputational value from the charter's fame, which is not the same flow as the arrangement's yield, but they are unambiguously subsidized by continued reverence. popular_constitutionalist_movements sit near the full-target pole, pushed further by identity_locked exit. charter_invoking_litigants carry high directionality damped by mobile exit — they can and do reroute to modern instruments at will. uk_parliament and the judiciary occupy the middle: they administer the arrangement without paying into it or collecting from it, parliament with arbitrage-grade control it declines to exercise, the judiciary constrained by settled practice. colonial_subjects_denied_charter_rights were, historically, the fullest targets with the worst exit — the highest-burden seat the arrangement ever produced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — feudal exaction and punitive justice under an Angevin king — died with the tenurial system that produced it; the legislative record and the historiography agree on this from outside the beneficiary set. Yet the arrangement persists and even intensifies performatively. That is the classic resolved-mandate signature: status dead combined with a world that still rearranges around the artifact (heritage calendars, civic education, political rhetoric, movement identities). The classification disciplines two opposite misreadings. Reading the arrangement as a snare overstates it: no seat captures the yield, no machinery suppresses challengers, and nothing is defended when tested — inertia and affection, not enforcement, hold it up. Reading it as a rope understates it: no live coordination problem is solved; the memory-coordination performed is thin and increasingly detached from any restraint delivered. The piton designation fits the cost-asymmetry test precisely: parliament could repeal the residue or entrench it tomorrow at trivial mechanical cost, but the benefit of fixing accrues diffusely to civic honesty while the perceived cost — unsettling parliamentary supremacy and the commemorative economy — lands on the one seat that bears none of the harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_rule_dispute,
    'Does the charter''s normative force transmit across the 1215-to-present boundary at all — the single structural point on which this reading (no transmission) diverges from the living-constitutionalism sibling (transmission through juridical precedent) and the parliamentary-sovereignty sibling (transmission only through enactment)?',
    'Comparative doctrinal survey: whether any apex court has ever grounded a holding on un-enacted charter text, as opposed to citing it as background; and whether any legislature has treated the un-repealed residue as self-executing restraint.',
    'Demonstrated transmission collapses this reading into the living-constitutionalism sibling and empties its victim set; confirmed non-transmission stabilizes this story''s epsilon and type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_rule_dispute, conceptual, 'Location of the inter-reading disagreement: the transmission rule for inherited normative force.').

omega_variable(
    piton_vs_snare_capture_ambiguity,
    'Is the arrangement inert by exhaustion (nobody maintains the charter''s inertness; it persists by inertia and commemoration) or quietly maintained because powerful seats prefer it (which would make the arrangement a defended extraction with the executive as capturer)?',
    'Trace defensive behavior: do government briefs, legislative drafting offices, or court practices actively oppose charter-based claims when raised, or do they merely decline to act? Active defense under challenge indicates maintenance; mere neglect indicates inertia.',
    'Evidence of active defense flips the classification toward a defended arrangement with modern_executive_governments as the named gain-flow seat; confirmed neglect confirms the atrophied-inertia profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_capture_ambiguity, empirical, 'Whether the arrangement''s persistence reflects exhaustion or quiet preference.').

omega_variable(
    symbolic_legitimation_yield,
    'How much discretionary space and public deference does invocation of the charter''s prestige actually purchase — deference that would not exist if the invocation were understood as referring to a spent medieval settlement?',
    'Political-science measurement: survey and experimental work on the effect of charter-invocation rhetoric on compliance, trust, and acceptance of executive action.',
    'Sets the scale of the arrangement''s principal yield; a negligible yield drops effective extractiveness toward the coordination-cost floor and softens the victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_legitimation_yield, empirical, 'Magnitude of the myth''s legitimating yield, the main quantity behind the extractiveness score.').

omega_variable(
    colonial_exclusion_counterfactual,
    'Would the doctrine of the charter''s limited reach have developed differently if colonial petitioners who invoked it had been seated in the forums that fixed its territorial and temporal limits?',
    'Archival study of imperial litigation and petitions invoking the charter, and of the administrative reasoning that refused them.',
    'A documented suppressed-counterfactual raises the historical extractiveness of the arrangement and adds a historical victim bloc to the structural record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_exclusion_counterfactual, empirical, 'Weight of the excluded colonial voice in the arrangement''s historical formation.').

omega_variable(
    framing_text_vs_restraint_tradition,
    'Is the kernel best framed as the fixed charter text (whose clauses are individually spent or surviving) or as the restraint tradition the charter inaugurated (which persists in whatever instrument carries it)? The two framings sort the same history differently.',
    'Test both framings against the classification outputs: under the text-framing the arrangement is a spent statute with ceremonial residue; under the tradition-framing the restraint function migrated to successor instruments and this story classifies only the vacated shell.',
    'Under the tradition-framing, this story''s victim set thins (the restraint function is not lost, only relocated) and the extractiveness attributable to THIS arrangement falls; signals guiding the choice were the statutory focus of the obsolescence claim itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_text_vs_restraint_tradition, conceptual, 'Framing under-determination in the commitment-system structure of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement(magn_tr_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 60, 0.72).
narrative_ontology:measurement(magn_tr_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 80, 0.78).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 100, 0.82).
narrative_ontology:measurement(magn_tr_t120, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 120, 0.84).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(magn_be_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(magn_be_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(magn_be_t120, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 120, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_constraint_authority__feudal_obsolescence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the authority of Magna Carta' decomposes into three structurally distinct claims about whether and how a 1215 compact's normative force reaches present institutions: transmission through juridical interpretation (living_constitutionalism_reading), transmission only through parliamentary enactment (parliamentary_sovereignty_reading), and no transmission (this story). Each carries its own epsilon, beneficiary/victim structure, and type; they form one constraint family joined by network edges. This story authors epsilon for the standing arrangement — ceremonial retention with operative inertness — assessed by the obsolescence reading's own lights; the upstream historical record (the spent-feudal-content findings shared by all three readings) feeds this story, while this story's inertness finding is cited by the living-constitutionalism sibling as the grievance its reading exists to remedy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
