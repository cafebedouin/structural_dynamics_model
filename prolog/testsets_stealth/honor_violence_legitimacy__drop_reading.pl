% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Gentlemanly Honor-Violence Legitimacy Structure (Drop Reading: Practice Suppressed, Legitimacy Intact)
 *   domain: historical sociology / legal anthropology / commitment systems
 *
 * SUMMARY:
 *   A single colloquial label — the decline of dueling — decomposes, per the
 *   epsilon-invariance principle, into structurally distinct claims; this
 *   story authors one of them: the drop reading of the
 *   honor_violence_legitimacy kernel. The standing arrangement under contest
 *   is the gentlemanly honor-violence legitimacy structure: the customary
 *   code duello by which the British gentlemanly class (aristocracy, officer
 *   corps, professions) adjudicated status disputes through rule-governed
 *   private combat. Over the interval 1770-1860 (interval units are years
 *   from 1770), the arrangement's practice collapsed — from a peak of
 *   frequent recorded duels to effective extinction — while its legitimacy
 *   persisted: juries declined to convict, the class kept the challenge
 *   protocol and satisfaction vocabulary alive, and the code remained the
 *   reference framework against which gentlemanly conduct was measured. The
 *   drop reading attributes the practice collapse entirely to rising external
 *   costs — criminal prosecution, military regulations, professional
 *   jeopardy, reform campaigns — and holds that had those costs been lifted,
 *   the practice would have resumed. The constraint this story classifies is
 *   therefore a live-but-suppressed structure: a tangled rope with falling
 *   throughput, not a dead letter. Sibling readings — contraction_reading
 *   (honor itself redefined so the code became structurally unthinkable) and
 *   composite_reading (both mechanisms simultaneous) — are separate
 *   constraints with their own epsilon, victims, and classifications, linked
 *   through the network. KEY AGENTS (by structural relationship): -
 *   gentlemanly_class: primary beneficiary (organized/identity_locked) — the
 *   status order the code constitutes; maintains its legitimacy while
 *   supplying its casualties - code_custodians: agenda_setter
 *   (organized/identity_locked) — seconds, club committees, regimental
 *   arbiters who administer the code and adjudicate satisfaction -
 *   officer_corp: dual beneficiary/payer (organized/constrained) — the most
 *   frequent duelists; the code regulated their internal disputes and
 *   consumed their members - duel_participants: primary payer
 *   (moderate/constrained) — bear the deaths, wounds, and legal jeopardy when
 *   the code triggers - honor_pressured_gentlemen: primary payer
 *   (moderate/identity_locked) — men coerced by honor obligations against
 *   their preference - widows_and_families: payer (powerless/constrained) —
 *   bear the deaths' consequences with no standing in the honor forums -
 *   commoner_classes: excluded (powerless/constrained) — outside the code's
 *   protections and governance; bear the class order it maintains -
 *   anti_dueling_campaigners: excluded (organized/constrained) — the reform
 *   coalition that built the external costs from forums the code does not
 *   recognize - criminal_justice_system: observer (institutional/analytical)
 *   — the prosecute-but-rarely-convict pattern holding the drop equilibrium
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.48).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.5).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Gentlemanly Honor-Violence Legitimacy Structure (Drop Reading: Practice Suppressed, Legitimacy Intact)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical sociology / legal anthropology / commitment systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '144f894e-8998-4bf1-9d73-2fb412d38e55').
narrative_ontology:cs_kernel_codification('144f894e-8998-4bf1-9d73-2fb412d38e55', distributed).
narrative_ontology:cs_authority_grounding('144f894e-8998-4bf1-9d73-2fb412d38e55', practice).
narrative_ontology:cs_interpretation_layer_present('144f894e-8998-4bf1-9d73-2fb412d38e55').
narrative_ontology:cs_reading_relation('144f894e-8998-4bf1-9d73-2fb412d38e55', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('144f894e-8998-4bf1-9d73-2fb412d38e55', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('144f894e-8998-4bf1-9d73-2fb412d38e55', foundational, legitimacy_practice_separability).
narrative_ontology:cs_axiom_status(legitimacy_practice_separability, holdable).
narrative_ontology:cs_axiom_grounding('144f894e-8998-4bf1-9d73-2fb412d38e55', legitimacy_practice_separability, empirically_contingent).
narrative_ontology:cs_axiom('144f894e-8998-4bf1-9d73-2fb412d38e55', foundational, external_costs_decline_sufficiency).
narrative_ontology:cs_axiom_status(external_costs_decline_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('144f894e-8998-4bf1-9d73-2fb412d38e55', external_costs_decline_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('144f894e-8998-4bf1-9d73-2fb412d38e55', operative_code_duello_framework).
narrative_ontology:cs_drift_state('144f894e-8998-4bf1-9d73-2fb412d38e55', mid_victorian_practice_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('144f894e-8998-4bf1-9d73-2fb412d38e55', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, gentlemanly_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, officer_corp).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, duel_participants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, honor_pressured_gentlemen).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, widows_and_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, gentlemanly_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, officer_corp).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, customary_honor_adjudication_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The aristocratic and officer class whose standing the code duello constitutes. It maintains the code's legitimacy — the challenge protocol, the satisfaction vocabulary, the fencing apparatus — as constitutive of gentlemanly identity, and collects the coordination benefit: a terminal, rule-governed procedure for status disputes that preempts feud and keeps dishonor out of the courts. It also supplies the code's casualties: its sons fight, are wounded, and are hanged or ruined when prosecutions succeed. Exit from the code is exit from gentility itself. As external costs rose across the interval, the class's practice fell faster than its legitimacy — it kept the code thinkable while its members stopped fighting.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, gentlemanly_class, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, gentlemanly_class, payer).

% The regimental officer corps — historically the densest dueling population. The code regulated promotion quarrels, table talk, and contested words inside a closed professional world where courts were dishonorable and command authority could not reach private satisfaction. Officers collected the benefit (a working dispute procedure inside the mess) and paid the heaviest toll (the majority of recorded duel deaths were officers). Military regulations against dueling, with dismissal the penalty, raised the professional cost of fighting while regimental opinion continued to demand it; the corps lived that contradiction for a generation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, officer_corp, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, officer_corp, payer).

% The seconds, dueling-club committees, regimental honor arbiters, and the editors who printed challenge correspondence. They administer the code: they set the terms of satisfaction, adjudicate apology adequacy, arrange weapons and ground, and decide when a matter is closed. Their authority exists only inside the code's framework; they collect standing and office from running it. As practice thinned, their work shifted from arranging fights to maintaining the framework — publishing the forms, training fencers, keeping the challenge protocol available — and they framed the decline throughout as external constraint on a valid code, never as the code's failure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, code_custodians, agenda_setter,
    organized, biographical, identity_locked, national).

% The men who stand on the ground: challenged and challenging. When the code triggers, they bear death, maiming, prosecution, and — for survivors — the legal and professional jeopardy that followed. Some fight willingly, collecting honor and satisfaction when they prevail; others fight because the alternative is dishonor. Their exit at the moment of challenge is costly in either direction: refuse and be cut by the class, or fight and face bullets and assizes. Across the interval their numbers thin — the population facing the ground shrinks faster than the population subject to the code.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, duel_participants, payer,
    moderate, immediate, constrained, regional).

% Men subject to the code's demands who would prefer not to fight: the religiously scrupulous, the professionally exposed, the simply afraid. For most of the interval their refusal was identity-unthinkable — a gentleman who declined satisfaction ceased, in the class's eyes, to be a gentleman, and the code's enforcement made that cost real. Late in the interval the external-cost environment opened a partial, honorable exit: citing legal jeopardy or professional ruin became an accepted ground for declining without full dishonor. The pressure on them is the arrangement's core extraction — the code converts their private preference into a public test they cannot quietly refuse.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_pressured_gentlemen, payer,
    moderate, biographical, identity_locked, national).

% The families of the killed and ruined. They bear the deaths, the lost incomes, and the social aftermath, and they have no standing anywhere in the code's forums — the honor system that consumed their husbands and sons never admitted their voice. Their testimony enters the historical record as private grief and, occasionally, as reform material; within the arrangement itself they are silent bearers of its costs.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, widows_and_families, payer,
    powerless, biographical, constrained, local).

% The non-gentlemanly population. The code's protections and obligations never extended to them — a laborer's quarrel was assault, not an affair of honor — while the status order the code maintained rested on their subordination. They sit outside the conversation the code adjudicates: they would object that the arrangement is a class privilege licensing for gentlemen a violence that would hang them, and indeed commoners who killed in quarrels were prosecuted without the jury leniency duelists received. Their exclusion is constitutive: the code's boundary work is part of what it does.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, commoner_classes, excluded,
    powerless, generational, constrained, national).

% Evangelical societies, Utilitarian reformers, the Anti-Dueling Association and its pledge, reforming military commanders, and monarchs who refused pardon. They compiled the casualty lists, prosecuted the cases, wrote the regulations, and preached the sermons that raised the external costs this reading names as the decline's cause. They operate entirely through courts, Parliament, pulpits, and the press — forums the code's own adjudication does not recognize as competent in honor matters; within the code's internal conversation their objections were inadmissible, answered by silence or by a challenge. They bear real costs for the position: social cut, accusation of cowardice by proxy, career friction.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, anti_dueling_campaigners, excluded,
    organized, generational, constrained, national).

% The assizes, the prosecutors, and the juries. Their pattern defines the drop equilibrium: formal hostility — manslaughter and murder indictments, the occasional capital conviction — coexisting with routine jury acquittal or lenient verdicts that preserved the code's de facto legitimacy. They see the whole structure: the class's private violence governance, the reform pressure, the nullification. Their record is the best external evidence for the drop reading: the law's costs rose for a generation while juries declined to make them real.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, criminal_justice_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, gentlemanly_class).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The code duello solved a real coordination problem for the gentlemanly class: it provided a rule-governed, terminal procedure for resolving status disputes — insults, slights, contested words — that would otherwise escalate into feuds, lawsuits, or uncontrolled violence, regulating challenge, refusal, weapons, seconds, and satisfaction so that honor could be defended and disputes closed.
% TRANSFER_FUNCTION: Moves honor-satisfaction and standing to the vindicated party and away from the loser or refuser; moves lethal and legal risk onto the participants and their families; and moves deference within the class — observance of the code marks and maintains membership, while refusal transfers standing from the refuser to the enforcement consensus.
% ABSENT_VOICES: The dead cannot speak; widows' grief entered the record as private testimony and reform material, never as argument inside the honor forums. Commoners were outside the code's protections and its adjudication while subject to the class order it maintained — their objection (a class privilege licensing for gentlemen a violence that would hang them) was structurally inadmissible where the code was administered. Inside the class, conscientious objectors could not voice refusal without invoking the dishonor the code reserves for refusers: their dissent was unspeakable in the code's own conversation and migrated to pulpits, pamphlets, and Parliament. The unanimity the honor forums displayed was therefore partly manufactured by the absence of these seats.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight in its drop-state form, the immediate behavioral change would be small — few duels were being fought — but the rearrangement would be real and mostly conceptual-institutional: the class would lose its terminal dispute procedure and its honor vocabulary, pending disputes would redistribute across courts, apologies, and private violence, the anti-dueling legal apparatus and reform societies would lose their object, and the fencing rooms and honor literature would lose their referent. The code remains the framework against which gentlemanly conduct is measured; removing it forces a redefinition of gentlemanly standing — which is precisely the contraction sibling's world. The drop reading's disappearance is the contraction reading's arrival.
% FOUNDING_PROBLEM: The gentlemanly class needed a way to defend status and settle disputes without either uncontrolled violence or the dishonor of legal recourse: the founding problem was how to make private violence among gentlemen rule-governed, terminal, and bounded — protecting the class's internal peace and its distinction from both feud and common law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the anti-dueling reform literature (which took the founding problem seriously enough to argue that law had superseded it), by military regulation preambles that treated the code's dispute-resolution function as real but superseded, and by legal historians of the feud-to-duel transition. The custodians' own attestations (dueling manuals, club records) are the beneficiary-side account. No party outside the honor economy attests that the founding problem remains live in its original form — the outside corroboration is of the problem's historical reality, not of any continuing need for dueling as its solution.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics describe the arrangement's drop-state operation (end of interval). Extractiveness 0.48: realized extraction has fallen with practice, but the structure still converts private preference into public tests — the honor-pressured payer bears coercion whenever the code triggers, participants bear lethal and legal risk, and the class bears the maintenance cost of keeping the framework available; epsilon is authored for the standing arrangement as the drop reading sees it, not for the practice level alone. Suppression 0.50, authored raw and unscaled (only extraction is scaled by directionality and scope): the code's hold shifted from compelled combat to compelled ritual — an insult still demands response, but the external-cost environment legitimized the honorable legal excuse, softening compulsion without dissolving it; the mechanism is both structural (class enforcement, regimental penalty) and internalized (identity fusion — see omega honor_suppression_internalization). Theater 0.42: the activity mix has shifted toward maintenance — manuals, fencing rooms, challenge correspondence preserved as culture — but the availability is real rather than vestigial, below the piton band. Accessibility collapse 0.42: alternatives (courts, apology, professional discipline) opened at modest honor cost, which is precisely how practice fell without the framework falling. Resistance 0.60: the external costs ARE institutionalized resistance — prosecution, military regulation, the pledge societies; note the coalition point — the code's enforcement was always vulnerable to coordinated refusal, and the Anti-Dueling Association's mass pledge was exactly that: a coalition of the code's own subjects collectively renouncing enforcement, which individual refusers could not survive alone. All three series share one time grid (t = 0, 15, 30, 45, 60, 75, 90); extractiveness rises to its Napoleonic-era peak before the external-cost regime bends it down. Suppression_requirement is authored because the story traces an enforcement transformation (compelled combat to compelled ritual), not merely metric drift. The coercion grid is authored because the drop reading IS a level-differentiation claim: at the structural level the norm's hold is maintained (suppression 0.50 rising to 0.55 as custodians curate availability) while individual-level compulsion collapses (0.72 to 0.50) — the same arrangement, read at different levels, is the drop reading in grid form.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the custodian and class seats the arrangement is a valid framework temporarily suppressed — the drop reading is literally the honor culture's own self-understanding, maintained in the fencing rooms and the acquittal record. From the honor-pressured payer seat the same structure is a standing trap: practice's rarity is no consolation for a demand that, when it comes, cannot be quietly refused. The officer corps lives both at once — the code regulates its internal order and consumes its members. The criminal-justice seat sees an equilibrium, not a decline: prosecutors supply the costs, juries decline to make them real. Identity lock is the pivot of the divergence: gentlemanly identity is constituted by the code (a gentleman IS one who answers), so the class seat cannot experience the arrangement as optional, while the excluded seats — commoners, reformers — experience it from outside as a class privilege. If the identity frame broke, as evangelical conversion broke it for individual officers, the payer seat's coercion would dissolve without any change in the code's text — which is why the reformers targeted conscience and collective pledge rather than the code's rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: gentlemanly_class and code_custodians sit near the beneficiary end — the class collects the dispute-order and the status economy; custodians collect office and standing from administering the code. officer_corp is genuinely dual (beneficiary of the internal procedure, heaviest payer of casualties) and derives mid-range. Victim declarations drive high d: duel_participants and honor_pressured_gentlemen sit near the target end, with identity_locked exit pushing the honor-pressured furthest toward full-target — their coercion is the arrangement's core extraction; widows_and_families are high-d payers with no standing anywhere in the arrangement's governance. The excluded seats sit outside the direct flow: commoner_classes bear the class order the code maintains but are declared neither beneficiary nor victim, so their directionality rests on the power-atom fallback — an honest residual this story does not paper over with an override, since the schema's override mechanism is keyed by power atom and would mis-specify the other powerless seat (widows_and_families) to adjust this one. Suppression is authored raw; the engine scales extraction by directionality and by national scope (verification difficulty at national scale modestly amplifies effective extraction). No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the structural relationships the seats actually hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton pull is real and must be named: practice has collapsed, the theater share has risen toward half, and the custodians could adapt the framework at little cost to themselves. What blocks the piton classification is the maintenance structure: the code is kept available by agents who benefit from its availability in an identity-constitutive way — the class's legitimacy maintenance is a real benefit stream, not inertia — and the drop reading's counterfactual (practice would resume if costs lifted) distinguishes a suppressed tangled rope from a dead one. The classification also blocks two flattening errors. Calling this a snare erases the genuine coordination function: the code displaced feud and kept gentlemanly violence rule-governed and terminal, and the historical record of what it replaced is the coordination evidence. Calling it a rope erases the coerced deaths and the identity-locked payers. The R5 genealogy is contested rather than dead: the founding problem (rule-governed satisfaction for gentlemanly disputes) is attested as historically real by sources outside the beneficiary set — reform literature, military regulation preambles, legal history — but no outside party attests it still requires this solution. Founding_problem_status=contested paired with disappearance_verdict=world_rearranges does not trip the dead-plus-rearranges mismatch flag; the mandatrophy question stays open, exactly as the drop reading holds it open — this arrangement is neither resolved nor resolved-against, but suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_availability,
    'This story instantiates the drop_reading of the honor_violence_legitimacy kernel. Is the post-collapse arrangement one of intact conceptual availability held under external suppression (this reading), or has the kernel moved to a sibling state — contraction_reading (honor redefined; dueling structurally unthinkable) or composite_reading (both mechanisms operating simultaneously)? The disagreement is located at one structural element: whether the code duello''s conceptual availability survived the practice collapse.',
    'Historiographic test of post-collapse honor reasoning: challenge correspondence and satisfaction vocabulary in memoirs written after practice ended; jury acquittals of a practice scarcely anyone practiced; fencing-room revivals; whether contemporaries still reasoned within the code or had redefined honor to exclude violence.',
    'If contraction_reading is right, this story''s persistence claims overstate the standing arrangement — the live constraint is a different one with a different victim set, and this file''s epsilon should not be read as describing the operative arrangement. If composite_reading is right, this story and the contraction sibling describe phases of one transition and their network edges should be reweighted as sequential rather than rival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_index_availability, conceptual, 'Which reading of the honor_violence_legitimacy kernel the post-decline arrangement instantiates.').

omega_variable(
    external_cost_counterfactual,
    'Is the drop reading''s counterfactual true — would practice have resumed had external costs (prosecution, military regulation, professional jeopardy) been lifted, or had legitimacy already decayed beyond recall?',
    'Quasi-experiments in the external-cost regime: jurisdictions and periods where enforcement lapsed or was never serious (dueling''s persistence on the Continent where legal risk ran lower); post-war relaxations; the handful of late British cases where prosecution failed — did challenges and meetings rise where costs fell?',
    'If practice rebounds when costs fall, the drop reading holds and the suppressed-tangled-rope reading stands. If it does not rebound, the decline was conceptual (contraction_reading), this story''s persistence claims fail, and the constraint should be re-read piton-ward or re-authored under the sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_cost_counterfactual, empirical, 'Whether external costs, rather than conceptual transformation, account for the practice collapse.').

omega_variable(
    jury_nullification_legitimacy,
    'Does the persistence of acquittals and lenient verdicts evidence genuine structural legitimacy (the code remained valid in the class''s eyes), or merely sentiment for a practice already fading — legitimacy as nostalgia rather than structure?',
    'Compare acquittal rates for duel killings against comparable non-duel killings across the interval, and track whether acquittal leniency persisted after practice had effectively ceased: juries acquitting a living practice show structure; juries acquitting a dead one show memory.',
    'Genuine legitimacy sustains the constraint''s conceptual availability and supports the drop reading''s persistence claim; hollow acquittals mean the legitimacy component was decaying in step with practice, moving the end state piton-ward and narrowing the gap between this reading and the contraction sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jury_nullification_legitimacy, empirical, 'Whether jury leniency measured live legitimacy or fading sentiment.').

omega_variable(
    honor_suppression_internalization,
    'Is the honor code''s remaining suppression of its subjects structural (class enforcement, regimental penalty, the cut direct) or internalized (identity fusion — the gentleman for whom refusal is unthinkable regardless of enforcement)?',
    'Post-exit trajectory: men who left the gentlemanly sphere by demotion, emigration, or class descent, and converts to evangelical scruple — did the honor compulsion persist after the enforcement environment was gone? Internalized suppression travels with the agent; structural suppression does not.',
    'If substantially internalized, effective suppression exceeds the structural measure and the identity-locked payer seats carry the code beyond its enforcement reach — raising the true extraction on those seats; if structural, the opening of honorable outs (the legal-risk excuse) should have dissolved compulsion faster than observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_suppression_internalization, empirical, 'Structural versus internalized mechanism of the code''s remaining coercive hold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drop_reading_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(drop_reading_tr_t0, observed).
narrative_ontology:measurement(drop_reading_tr_t15, honor_violence_legitimacy__drop_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(drop_reading_tr_t15, observed).
narrative_ontology:measurement(drop_reading_tr_t30, honor_violence_legitimacy__drop_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(drop_reading_tr_t30, observed).
narrative_ontology:measurement(drop_reading_tr_t45, honor_violence_legitimacy__drop_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(drop_reading_tr_t45, observed).
narrative_ontology:measurement(drop_reading_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(drop_reading_tr_t60, observed).
narrative_ontology:measurement(drop_reading_tr_t75, honor_violence_legitimacy__drop_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(drop_reading_tr_t75, observed).
narrative_ontology:measurement(drop_reading_tr_t90, honor_violence_legitimacy__drop_reading, theater_ratio, 90, 0.42).
narrative_ontology:measurement_basis(drop_reading_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(drop_reading_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(drop_reading_be_t0, observed).
narrative_ontology:measurement(drop_reading_be_t15, honor_violence_legitimacy__drop_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(drop_reading_be_t15, observed).
narrative_ontology:measurement(drop_reading_be_t30, honor_violence_legitimacy__drop_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(drop_reading_be_t30, observed).
narrative_ontology:measurement(drop_reading_be_t45, honor_violence_legitimacy__drop_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(drop_reading_be_t45, observed).
narrative_ontology:measurement(drop_reading_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement_basis(drop_reading_be_t60, observed).
narrative_ontology:measurement(drop_reading_be_t75, honor_violence_legitimacy__drop_reading, base_extractiveness, 75, 0.53).
narrative_ontology:measurement_basis(drop_reading_be_t75, observed).
narrative_ontology:measurement(drop_reading_be_t90, honor_violence_legitimacy__drop_reading, base_extractiveness, 90, 0.48).
narrative_ontology:measurement_basis(drop_reading_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(drop_reading_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement_basis(drop_reading_su_t0, observed).
narrative_ontology:measurement(drop_reading_su_t15, honor_violence_legitimacy__drop_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(drop_reading_su_t15, observed).
narrative_ontology:measurement(drop_reading_su_t30, honor_violence_legitimacy__drop_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(drop_reading_su_t30, observed).
narrative_ontology:measurement(drop_reading_su_t45, honor_violence_legitimacy__drop_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(drop_reading_su_t45, observed).
narrative_ontology:measurement(drop_reading_su_t60, honor_violence_legitimacy__drop_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(drop_reading_su_t60, observed).
narrative_ontology:measurement(drop_reading_su_t75, honor_violence_legitimacy__drop_reading, suppression_requirement, 75, 0.54).
narrative_ontology:measurement_basis(drop_reading_su_t75, observed).
narrative_ontology:measurement(drop_reading_su_t90, honor_violence_legitimacy__drop_reading, suppression_requirement, 90, 0.5).
narrative_ontology:measurement_basis(drop_reading_su_t90, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=90
narrative_ontology:measurement(drop_reading_grid_01, honor_violence_legitimacy__drop_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_01, observed).
narrative_ontology:measurement(drop_reading_grid_02, honor_violence_legitimacy__drop_reading, accessibility_collapse(class), 90, 0.4).
narrative_ontology:measurement_basis(drop_reading_grid_02, observed).
narrative_ontology:measurement(drop_reading_grid_03, honor_violence_legitimacy__drop_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement_basis(drop_reading_grid_03, observed).
narrative_ontology:measurement(drop_reading_grid_04, honor_violence_legitimacy__drop_reading, accessibility_collapse(individual), 90, 0.25).
narrative_ontology:measurement_basis(drop_reading_grid_04, observed).
narrative_ontology:measurement(drop_reading_grid_05, honor_violence_legitimacy__drop_reading, accessibility_collapse(organizational), 0, 0.6).
narrative_ontology:measurement_basis(drop_reading_grid_05, observed).
narrative_ontology:measurement(drop_reading_grid_06, honor_violence_legitimacy__drop_reading, accessibility_collapse(organizational), 90, 0.35).
narrative_ontology:measurement_basis(drop_reading_grid_06, observed).
narrative_ontology:measurement(drop_reading_grid_07, honor_violence_legitimacy__drop_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement_basis(drop_reading_grid_07, observed).
narrative_ontology:measurement(drop_reading_grid_08, honor_violence_legitimacy__drop_reading, accessibility_collapse(structural), 90, 0.45).
narrative_ontology:measurement_basis(drop_reading_grid_08, observed).
narrative_ontology:measurement(drop_reading_grid_09, honor_violence_legitimacy__drop_reading, resistance(class), 0, 0.3).
narrative_ontology:measurement_basis(drop_reading_grid_09, observed).
narrative_ontology:measurement(drop_reading_grid_10, honor_violence_legitimacy__drop_reading, resistance(class), 90, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_10, observed).
narrative_ontology:measurement(drop_reading_grid_11, honor_violence_legitimacy__drop_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement_basis(drop_reading_grid_11, observed).
narrative_ontology:measurement(drop_reading_grid_12, honor_violence_legitimacy__drop_reading, resistance(individual), 90, 0.6).
narrative_ontology:measurement_basis(drop_reading_grid_12, observed).
narrative_ontology:measurement(drop_reading_grid_13, honor_violence_legitimacy__drop_reading, resistance(organizational), 0, 0.3).
narrative_ontology:measurement_basis(drop_reading_grid_13, observed).
narrative_ontology:measurement(drop_reading_grid_14, honor_violence_legitimacy__drop_reading, resistance(organizational), 90, 0.55).
narrative_ontology:measurement_basis(drop_reading_grid_14, observed).
narrative_ontology:measurement(drop_reading_grid_15, honor_violence_legitimacy__drop_reading, resistance(structural), 0, 0.25).
narrative_ontology:measurement_basis(drop_reading_grid_15, observed).
narrative_ontology:measurement(drop_reading_grid_16, honor_violence_legitimacy__drop_reading, resistance(structural), 90, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_16, observed).
narrative_ontology:measurement(drop_reading_grid_17, honor_violence_legitimacy__drop_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement_basis(drop_reading_grid_17, observed).
narrative_ontology:measurement(drop_reading_grid_18, honor_violence_legitimacy__drop_reading, stakes_inflation(class), 90, 0.35).
narrative_ontology:measurement_basis(drop_reading_grid_18, observed).
narrative_ontology:measurement(drop_reading_grid_19, honor_violence_legitimacy__drop_reading, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement_basis(drop_reading_grid_19, observed).
narrative_ontology:measurement(drop_reading_grid_20, honor_violence_legitimacy__drop_reading, stakes_inflation(individual), 90, 0.3).
narrative_ontology:measurement_basis(drop_reading_grid_20, observed).
narrative_ontology:measurement(drop_reading_grid_21, honor_violence_legitimacy__drop_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement_basis(drop_reading_grid_21, observed).
narrative_ontology:measurement(drop_reading_grid_22, honor_violence_legitimacy__drop_reading, stakes_inflation(organizational), 90, 0.4).
narrative_ontology:measurement_basis(drop_reading_grid_22, observed).
narrative_ontology:measurement(drop_reading_grid_23, honor_violence_legitimacy__drop_reading, stakes_inflation(structural), 0, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_23, observed).
narrative_ontology:measurement(drop_reading_grid_24, honor_violence_legitimacy__drop_reading, stakes_inflation(structural), 90, 0.35).
narrative_ontology:measurement_basis(drop_reading_grid_24, observed).
narrative_ontology:measurement(drop_reading_grid_25, honor_violence_legitimacy__drop_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement_basis(drop_reading_grid_25, observed).
narrative_ontology:measurement(drop_reading_grid_26, honor_violence_legitimacy__drop_reading, suppression(class), 90, 0.45).
narrative_ontology:measurement_basis(drop_reading_grid_26, observed).
narrative_ontology:measurement(drop_reading_grid_27, honor_violence_legitimacy__drop_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement_basis(drop_reading_grid_27, observed).
narrative_ontology:measurement(drop_reading_grid_28, honor_violence_legitimacy__drop_reading, suppression(individual), 90, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_28, observed).
narrative_ontology:measurement(drop_reading_grid_29, honor_violence_legitimacy__drop_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement_basis(drop_reading_grid_29, observed).
narrative_ontology:measurement(drop_reading_grid_30, honor_violence_legitimacy__drop_reading, suppression(organizational), 90, 0.4).
narrative_ontology:measurement_basis(drop_reading_grid_30, observed).
narrative_ontology:measurement(drop_reading_grid_31, honor_violence_legitimacy__drop_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement_basis(drop_reading_grid_31, observed).
narrative_ontology:measurement(drop_reading_grid_32, honor_violence_legitimacy__drop_reading, suppression(structural), 90, 0.55).
narrative_ontology:measurement_basis(drop_reading_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the honor_violence_legitimacy kernel decomposes per the epsilon-invariance principle into three readings with distinct epsilon values, victim sets, and classifications. This story (drop_reading) authors the standing arrangement in its drop-state: practice rare, legitimacy intact — a suppressed tangled rope whose epsilon reflects real-but-diminished extraction. The contraction sibling authors the redefined-honor arrangement (dueling structurally unthinkable; different victim set; different epsilon). The composite sibling authors the overdetermined-transition arrangement (both mechanisms simultaneous). The readings are linked bidirectionally through affects_constraints; this reading's relation to contraction is ordered-phase (the drop-state's persistence is the condition whose dissolution constitutes the contraction-state), and its relation to composite is rival-weighting (composite holds this reading's external-cost mechanism as one component). The colloquial label the decline of dueling conflates all three; the family exists to keep them apart.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
