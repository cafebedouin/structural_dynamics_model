% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor-Satisfaction Mechanism (Dueling) — Contraction Reading
 *   domain: historical sociology / legal history / normative systems
 *
 * SUMMARY:
 *   For roughly three centuries the armed gentlemen of Europe governed their
 *   quarrels through a machinery of honor: an insult created an obligation,
 *   the obligation was discharged by rule-governed single combat, and the
 *   machinery — codes, seconds, courts of honor, ostracism of refusers — was
 *   administered by the honor class itself. This story instantiates the
 *   contraction reading of the contested kernel honor_satisfaction_mechanism:
 *   the claim that the machinery's end, in the generation after the Great
 *   War, was not a decline in frequency to a conceivable fringe but a
 *   category-level evacuation — the cognitive frame in which ritual combat
 *   could appear as a way to repair damaged honor dissolved, and with it the
 *   mechanism's existence as a possibility at all. The epsilon referent is
 *   the standing dueling arrangement during its operation, assessed by this
 *   reading's own lights; the sibling readings are separate constraints with
 *   their own epsilon and are not averaged here. Claim and metrics are
 *   independent authored facts: claimed_type records the mechanism's
 *   operating structure; the measurement series records its terminal
 *   signature — abrupt collapse, not a decay tail.
 *
 * KEY AGENTS:
 *   - gentleman_honor_class: agenda-setter and principal beneficiary (organized / identity_locked) — administered the honor economy through regimental custom and courts of honor, collected its status boundary, and paid its costs in its own members' blood; exit meant forfeiting the class's entire world
 *   - compelled_duelists: primary targets (moderate / identity_locked) — fought because refusal meant the death of the self the code had made them, whatever the risk to the body
 *   - duel_refusers: targets (moderate / trapped) — declined the field and bore the machinery of consequence: dismissal, ostracism, ruined standing; full exit required leaving the class
 *   - dueling_ritual_specialists: secondary beneficiaries (moderate / constrained) — seconds, fencing masters, and surgeons whose trade the code sustained
 *   - state_sovereign_authorities: nominal agenda-setter (institutional / constrained) — passed anti-dueling statutes for centuries and could not enforce them against the class's internal governance
 *   - anti_dueling_coalition: excluded voice (organized / constrained) — clergy, reformers, and societies whose objections had no standing inside the code they opposed
 *   - legal_historical_analysts: analytical observer — reconstructs the machinery's operation and weighs rival accounts of its end
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.68).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor-Satisfaction Mechanism (Dueling) — Contraction Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical sociology / legal history / normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'b197fdee-fae4-4334-8bf3-5a6dad932acd').
narrative_ontology:cs_kernel_codification('b197fdee-fae4-4334-8bf3-5a6dad932acd', distributed).
narrative_ontology:cs_authority_grounding('b197fdee-fae4-4334-8bf3-5a6dad932acd', practice).
narrative_ontology:cs_interpretation_layer_present('b197fdee-fae4-4334-8bf3-5a6dad932acd').
narrative_ontology:cs_reading_relation('b197fdee-fae4-4334-8bf3-5a6dad932acd', honor_satisfaction_mechanism__decline_reading, forecloses).
narrative_ontology:cs_reading_relation('b197fdee-fae4-4334-8bf3-5a6dad932acd', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('b197fdee-fae4-4334-8bf3-5a6dad932acd', foundational, possibility_space_evacuation_terminal).
narrative_ontology:cs_axiom_status(possibility_space_evacuation_terminal, holdable).
narrative_ontology:cs_axiom_grounding('b197fdee-fae4-4334-8bf3-5a6dad932acd', possibility_space_evacuation_terminal, empirically_contingent).
narrative_ontology:cs_axiom('b197fdee-fae4-4334-8bf3-5a6dad932acd', secondary, identity_frame_dissolution_precondition).
narrative_ontology:cs_axiom_status(identity_frame_dissolution_precondition, holdable).
narrative_ontology:cs_axiom_grounding('b197fdee-fae4-4334-8bf3-5a6dad932acd', identity_frame_dissolution_precondition, empirically_contingent).
narrative_ontology:cs_reference_frame('b197fdee-fae4-4334-8bf3-5a6dad932acd', practice_constituted_honor_category).
narrative_ontology:cs_drift_state('b197fdee-fae4-4334-8bf3-5a6dad932acd', post_great_war_interwar, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('b197fdee-fae4-4334-8bf3-5a6dad932acd', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, gentleman_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, dueling_ritual_specialists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, compelled_duelists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, duel_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, gentleman_honor_class).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, honor_as_satisfiable_substance).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, code_duello_procedural_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European officer corps and gentry whose standing rested on a code of personal honor. Through regimental custom, courts of honor, and written dueling codes the class defined what counted as an insult, what repaired one, and what befell a man who declined the field. It administered the machinery and collected its principal product: a bounded, exclusive status order separating gentlemen from everyone else. Its own members paid the code's costs in blood — sons, brothers, and breadwinners killed or maimed in affairs of honor. Leaving the class meant forfeiting commission, marriage prospects, and family standing, so its requirements were not experienced as optional.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, gentleman_honor_class, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__contraction_reading, gentleman_honor_class, payer).

% Officers and gentlemen called to the field by an insult or an accusation. Refusal was not a private choice but a public unmaking: dismissal from the service, exclusion from regimental society, unmarriageable sisters, a family name carried in disgrace. Most went to the ground because the alternative was the death of the self they had been raised to be, whatever the risk to the body. The survivors sent their own sons into the same economy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, compelled_duelists, payer,
    moderate, biographical, identity_locked, national).

% Men who declined the field on religious, prudential, or personal grounds while wishing to keep their place in the class whose code they refused. The machinery of consequence fell on them directly: forced resignations, broken engagements, decades of cold exclusion, careers ended by tribunals of their peers. Their only complete way out was leaving the class entirely, at the cost of everything the class conferred.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, duel_refusers, payer,
    moderate, biographical, trapped, national).

% Seconds, fencing masters, dueling surgeons, and code consultants who lived from the machinery: engaged to arrange terms, certify weapons, attend wounds, and advise on points of honor. Their income and professional standing depended on the code staying in force; they could not keep it alive alone and had every reason to defend it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, dueling_ritual_specialists, beneficiary,
    moderate, biographical, constrained, national).

% Crowns, legislatures, and courts that claimed the monopoly of lawful violence and passed statutes against dueling from the sixteenth century onward. The statutes went persistently unenforced: juries declined to convict, sovereigns pardoned the convicted, prosecutors would not pursue men the code had required to fight. Every attempt to make the prohibition real cost the state legitimacy among the very classes it depended on, so the legal agenda stayed subordinate to the honor class's internal governance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_sovereign_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Clergy, humanitarian reformers, anti-dueling societies, and pamphleteers who condemned the code and organized against it for two centuries. Their arguments — that the code sanctified killing, that it ruined families, that a Christian or a prudent man must refuse — had no standing inside the machinery they opposed: the code recognized only honor-relevant considerations, so objection could be voiced only as provocation. They operated in pulpits, pamphlets, and legislatures, never in the courts of honor where the code's meaning was settled.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, anti_dueling_coalition, excluded,
    organized, generational, constrained, continental).

% Historians and historical sociologists who reconstruct the code's operation from regimental records, trial transcripts, correspondence, and the dueling literature, and who weigh rival accounts of how and why the practice ended.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, legal_historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__contraction_reading, gentleman_honor_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the honor class the machinery solved a real dispute-ordering problem: it converted quarrels between armed, status-anxious equals into rule-governed, bounded single-combat affairs with fixed procedure — challenge, seconds, terms, a field, a verdict of satisfaction — and a defined endpoint, in place of feuds and vendettas that would have consumed the class from inside. It standardized what counted as an insult and what repaired one across regiments, courts, and national borders.
% TRANSFER_FUNCTION: It moved mortal risk and bodily harm from gentlemen who had given or taken offense onto the field, paid by both parties and collected by the survivors; it moved adjudication of status disputes out of state courts into the class's own tribunals; and it moved fees and standing to the machinery's specialists. The principal transfer was autonomy: the code decided for each member what an insult required, removing the option of letting an offense pass.
% ABSENT_VOICES: The refusers were present in the world but absent from the code's adjudication: the framework recognized only honor-relevant considerations, so religious and prudential objection had no standing and could be voiced only as ruin. The dead and maimed had no seat. And those outside the honor economy were absent entirely: a gentleman's honor was satisfied by a duel, but the servant, the civilian, or the social inferior struck or killed by a gentleman had no claim the machinery could hear.
% DISAPPEARANCE_RATIONALE: During the machinery's operation, overnight removal would have forced immediate rearrangement: no procedure for insults short of feud or lawsuit, regimental discipline stripped of its honor sanction, the specialists' trade gone, and the class boundary in need of redrawing by other means. After the terminal collapse the measurement series records, its absence required no replacement at all — the category of satisfiable honor dissolved with it. The world did not replace the mechanism; it stopped needing one, which is this reading's signature.
% FOUNDING_PROBLEM: A hereditary military-aristocratic class, armed and jealous of standing, in a world where the state could not or would not protect individual honor and private vengeance meant generational feud: the machinery was built to let that class repair damaged honor and end disputes through bounded, rule-governed single combat instead of vendetta.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the legal-historical and historical-sociological literature (Kiernan's The Duel in European History, Baldick's The Duel, Frevert's work on the German honor code), which reconstructs the founding problem from regimental and trial records; by the anti-dueling reform literature of the period itself, which conceded the feud-suppression function while denying the code's legitimacy; and by the post-1918 record — no modern institution claims a live need for honor-satisfaction, and no successor practice has taken up the machinery's function. No descendant of the beneficiary parties attests the problem is live.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58 at the referent (the mechanism in operation): the code took lives, autonomy, and fees from the class it governed, against a real service — bounded dispute resolution in place of feud. Suppression 0.68 is structural: courts of honor, regimental tribunals, and ostracism actively closed the refusal path, and non-violent settlement was dishonorable under the hard codes. Theater 0.22 at the referent — the operating mechanism was substantially functional; the temporal series then shows Goodhart drift (theater 0.18 rising to 0.52 by 1890) as ritual outlived function, before the terminal collapse. Accessibility_collapse 0.74: inside the honor frame, alternatives collapsed almost completely — apology without the field was dishonor, and the frame itself could be exited only at the cost of the self. Resistance 0.45: two centuries of statutes, church condemnation, and organized societies met jury nullification, elite solidarity, and identity-lock, and failed. All three series run on one shared six-point grid (approximately 1770-1920 at 30-year steps), each metric authored at every point. The terminal signature is this reading's evidence: extractiveness and enforcement do not decay toward a stable fringe floor — they fall off a cliff between 1890 and 1920, and theater collapses with them, because once the category is gone there is nothing left to perform. A smooth decay to a persistent floor would instead support the decline reading.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From inside the honor class the machinery was constitutive order — its members experienced compulsion as honor and the code's demands as their own identity speaking; from the refuser's seat the same machinery was coercion wearing a vocabulary; from the state's seat it was a standing sovereignty defect it could statute against but not enforce against; from the excluded reformers' seat it was an atrocity whose framework could not hear the objection. The contraction reading adds a fourth position: after the category's death, none of these seats exists — there is no one left inside the frame to experience the mechanism as anything, which is what distinguishes evacuation from suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (gentleman_honor_class, dueling_ritual_specialists) drive their d toward the beneficiary end; victim declarations (compelled_duelists, duel_refusers) drive theirs toward the target end, with identity_locked and trapped exit pushing the duelists and refusers toward full-target. The distinctive structure: extraction was internal to the beneficiary class — the class collected the status boundary and paid the mortality — so the honor class's declared secondary payer role pulls its d toward symmetric rather than letting it ride as a pure beneficiary seat. No directionality overrides are used: the beneficiary/victim declarations plus the secondary_role and exit data carry the dual position, and the derivation chain handles it. Suppression is authored as a raw structural property and is not scaled; extraction is scaled by the engine from directionality and the continental scope of the honor culture.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both faces of the mechanism in view: calling it a snare erases the genuine coordination (feud-bounding, standardized dispute procedure across regiments and borders) that made gentlemen consent to it; calling it a rope erases the coerced dead and the ruined refusers. The R5 interview resolves obsolescence cleanly: the founding problem — honor adjudication for a stateless armed class — is dead with the class that had it, corroborated externally by legal-historical scholarship and by the reform literature, both outside the beneficiary set. The mismatch check (dead founding problem paired with a world_rearranges verdict) raises a zombie candidate, and the temporal series resolves it: the zombie phase was terminal. Theater rose past 0.5 as the function hollowed, and then the whole structure collapsed rather than persisting as performance. That is this reading's difference from a piton: a piton is kept alive by theatrical maintenance after its function dies, but the honor mechanism was kept alive by a living identity category — and when the category died, nothing was left to maintain, not even the theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_disagreement_terminal_state,
    'For the kernel honor_satisfaction_mechanism, did the mechanism end by category-level evacuation — cognitively unthinkable, this contraction reading — by frequency decline to a conceivable fringe (decline_reading), or by plural mechanisms with no single owner (composite_reading)?',
    'Comparative analysis of post-1918 honor-class documents: whether duel-and-satisfaction vocabulary persisted as a formulable option (supporting decline_reading) or vanished from the conceptual repertoire (supporting contraction_reading); and whether single-mechanism attribution survives counterfactual analysis of state monopoly, insurance exclusions, and bourgeois norm diffusion (composite_reading).',
    'If contraction holds, the constraint is structurally absent at interval end and no residual constraint needs classifying; if decline holds, a residual fringe constraint persists and requires its own story with its own epsilon; if composite holds, no single terminal type exists and the kernel decomposes into per-mechanism stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_terminal_state, empirical, 'Committer omega: which sibling reading characterizes the mechanism''s terminal state; the disagreement is located in the structural character of the end state, not in the mechanism''s operation.').

omega_variable(
    unthinkability_vs_revulsion,
    'Does ''cognitively unthinkable'' mean the category dissolved — dueling no longer formulable as an act — or that it remained formulable but met automatic revulsion, i.e. an internalized prohibition?',
    'Discourse analysis of interwar honor-class writing: could a gentleman still describe a duel as a thing one might do (with disapproval), or had the description itself lost coherence?',
    'Category dissolution supports this reading''s structural-disappearance delta; mere revulsion means the constraint persists as identity-locked prohibition, the terminal metrics are misdated, and the story''s type at interval end is closer to an internalized-suppression residue than to absence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unthinkability_vs_revulsion, conceptual, 'Meaning of ''unthinkable'': evacuated possibility space versus internalized revulsion — the two states differ structurally even when behaviorally indistinguishable.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the dueling code''s hold on its participants structural (ostracism machinery, tribunal dismissal, career ruin) or internalized (a honor identity that made refusal inconceivable even where external consequences were absent)?',
    'Refuser-case outcomes: compare refusers with institutional protection (patronage, foreign commissions, wealth) who escaped penalty against refusers ruined despite external safety — the residual penalty after structural barriers are lifted measures internalization.',
    'If substantially internalized, the code''s suppression outlived its enforcement machinery and the terminal collapse required identity death, not enforcement lapse — supporting the contraction reading; if mostly structural, legal enforcement change alone could have sufficed, strengthening composite_reading''s state-monopoly component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized compulsion in the honor code''s grip on its own class.').

omega_variable(
    category_collapse_causation_direction,
    'Did the category collapse drive the practice''s disappearance, or did the practice''s death — the Great War discrediting ritual combat and destroying the martial-aristocratic stratum — drive the category collapse?',
    'Date the dissolution of satisfaction vocabulary in honor-class correspondence against the practice''s frequency curve in 1914-1930 records; if the vocabulary died while elderly class members still recalled dueling as personally available, the category led.',
    'If practice-death led, the contraction reading''s autonomy claim weakens and composite_reading''s war-shock component gains; if the category led, this reading owns the terminal mechanism rather than inheriting it from the war.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_collapse_causation_direction, empirical, 'Causal direction between category dissolution and practice disappearance at the terminal boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_contraction_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hsm_contraction_tr_t6, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(hsm_contraction_tr_t12, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(hsm_contraction_tr_t18, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(hsm_contraction_tr_t24, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(hsm_contraction_tr_t30, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 30, 0.04).

% Extraction over time
narrative_ontology:measurement(hsm_contraction_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hsm_contraction_be_t6, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(hsm_contraction_be_t12, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(hsm_contraction_be_t18, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(hsm_contraction_be_t24, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(hsm_contraction_be_t30, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 30, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(hsm_contraction_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(hsm_contraction_su_t6, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(hsm_contraction_su_t12, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(hsm_contraction_su_t18, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(hsm_contraction_su_t24, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(hsm_contraction_su_t30, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% The kernel honor_satisfaction_mechanism decomposes into three readings per the epsilon-invariance principle: this contraction story (terminal state = category-level evacuation of the possibility space), the decline reading (terminal state = conceivable fringe persistence at declining frequency), and the composite reading (plural mechanisms — state monopoly, bourgeois norms, insurance, category-shift — with no single owner). Each is a separate constraint with its own epsilon, beneficiaries, and classification; the sibling IDs are linked here. The contraction and decline readings make contradictory claims about the same terminal state and are related by foreclosure; the contraction reading supplies the category-shift component that the composite reading incorporates and is related to it by influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
