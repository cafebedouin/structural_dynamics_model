% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Moral Status (Sentience-With-Use Doctrine)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the contested
 *   animal-status kernel: animals are sentient beings whose interests
 *   generate real, legally cognizable constraints on human use, but those
 *   constraints operate as an exemption structure rather than a prohibition.
 *   Anti-cruelty statutes bite on gratuitous or excessive harm while carving
 *   out 'ordinary' industrial confinement, transport, slaughter, and research
 *   use as categorically outside the cruelty standard. This produces a
 *   hybrid: a genuine coordination function (courts and regulators need a
 *   workable standard; total prohibition and total permission are both
 *   unworkable extremes society has rejected) layered over an asymmetric
 *   extraction (the exemption boundary is drawn to preserve routine use, and
 *   animals inside 'ordinary use' categories bear costs the sentience premise
 *   itself would flag as morally significant, but the legal category exempts
 *   them from remedy). The rising extractiveness and theater-ratio series
 *   reflect an observed trend: welfare certification and compliance theater
 *   (audits, labels, standards bodies) have expanded over recent decades even
 *   as the scope of animals actually protected against the underlying scale
 *   of suffering has not proportionally grown — more paperwork and more
 *   'humane' branding accompanying, not displacing, an expanding industrial
 *   base.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: institutional beneficiary and co-drafter of the exemption boundary
 *   - biomedical_research_institutions: institutional beneficiary operating under welfare-minimization review
 *   - welfare_certification_bodies: organized beneficiary whose business model requires the constraint's continued middle position
 *   - farmed_animals_in_gratuitous_cruelty_cases: powerless, trapped payer — the class the reading actually protects
 *   - animals_in_uncertified_use_settings: powerless, trapped payer — the much larger class the reading's exemptions exclude from protection
 *   - consumers_of_animal_products: moderate-power beneficiary with genuine mobile exit (plant-based substitution)
 *   - animal_advocacy_organizations: excluded voice arguing the category boundary itself is the harm
 *   - legislators_and_regulators: institutional agenda-setters who draw and redraw the exemption line
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare Reading of Animal Moral Status (Sentience-With-Use Doctrine)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'b90f04ff-3c32-4659-be2b-d9bedbcc364e').
narrative_ontology:cs_kernel_codification('b90f04ff-3c32-4659-be2b-d9bedbcc364e', distributed).
narrative_ontology:cs_authority_grounding('b90f04ff-3c32-4659-be2b-d9bedbcc364e', distributed).
narrative_ontology:cs_reading_relation('b90f04ff-3c32-4659-be2b-d9bedbcc364e', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b90f04ff-3c32-4659-be2b-d9bedbcc364e', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('b90f04ff-3c32-4659-be2b-d9bedbcc364e', foundational, graduated_moral_status_by_sentience).
narrative_ontology:cs_axiom_status(graduated_moral_status_by_sentience, holdable).
narrative_ontology:cs_axiom_grounding('b90f04ff-3c32-4659-be2b-d9bedbcc364e', graduated_moral_status_by_sentience, empirically_contingent).
narrative_ontology:cs_axiom('b90f04ff-3c32-4659-be2b-d9bedbcc364e', secondary, ordinary_use_exemption_is_principled_not_arbitrary).
narrative_ontology:cs_axiom_status(ordinary_use_exemption_is_principled_not_arbitrary, holdable).
narrative_ontology:cs_axiom_grounding('b90f04ff-3c32-4659-be2b-d9bedbcc364e', ordinary_use_exemption_is_principled_not_arbitrary, conventional).
narrative_ontology:cs_reference_frame('b90f04ff-3c32-4659-be2b-d9bedbcc364e', post_cruelty_statute_compromise).
narrative_ontology:cs_drift_state('b90f04ff-3c32-4659-be2b-d9bedbcc364e', contemporary_industrial_scale_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b90f04ff-3c32-4659-be2b-d9bedbcc364e', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals_in_gratuitous_cruelty_cases).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals_in_uncertified_use_settings).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_grounds_moral_consideration).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, graduated_moral_status_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates confinement and slaughter systems within welfare-statute limits (space, stunning, transport rules) that it helped draft through trade-association lobbying. The welfare frame supplies legal cover and a marketing vocabulary ('humane,' 'certified') while leaving the underlying use — breeding, confining, killing for food — untouched. Compliance cost is a manageable line item, not an existential threat, because the statutes exempt ordinary practice from the cruelty standard applied to gratuitous harm.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter).

% Uses animals in regulated research under institutional review boards that apply welfare minimization ('the three Rs') rather than prohibition. The welfare reading lets research proceed with documented justification requirements that rarely block a proposed protocol; exit from animal models exists in principle (in vitro, computational) but is slow and career-costly to adopt.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, national).

% Administers standards and audits (cage-free, humane-certified, etc.) that operationalize the welfare reading and charges producers for certification. Its continued relevance depends on the constraint remaining exactly where it is — enough regulation to need certifiers, not so much that the underlying use is foreclosed.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_certification_bodies, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_certification_bodies, agenda_setter).

% Are the class the welfare reading actually protects: individuals subjected to harm beyond what confinement/slaughter/research require are the ones whose interests the anti-cruelty statutes can vindicate in court. They have no exit and no voice; their situation is litigated, when it is litigated, entirely through human proxies.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals_in_gratuitous_cruelty_cases, payer,
    powerless, immediate, trapped, local).

% Standard industrial confinement, transport, and slaughter — the routine, uncertified baseline welfare law explicitly permits — falls on this group. Their suffering is real by the welfare reading's own sentience premise but does not register as a legal harm because the exemption structure defines ordinary use as non-cruel by definition, not by evidence of reduced suffering.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals_in_uncertified_use_settings, payer,
    powerless, immediate, trapped, local).

% Buys animal products at prices that do not internalize the full cost of welfare-compliant treatment, and can choose 'humane certified' labels for moral reassurance without the underlying use being questioned. Genuine plant-based exit exists and is increasingly accessible, distinguishing this seat from the trapped animal seats.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, national).

% Argue from outside the welfare framework's own premises that sentience should ground rights against use, not merely against excess cruelty within use. They participate in the legal and legislative process but their core objection — that the categories 'ordinary use' and 'cruelty' are drawn in the industry's interest — is structurally excluded from what welfare statutes can adjudicate.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Draft and revise the welfare statutes and exemption carve-outs, balancing agricultural-lobby input against advocacy pressure and public sentiment. They set where the line between 'welfare violation' and 'ordinary practice' falls, and that line-drawing is the entire content of the constraint.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, legislators_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable legal and moral vocabulary that lets a society continue routine animal use (food, research, labor, companionship) while giving courts, regulators, and consumers a standard for distinguishing acceptable use from gratuitous or excessive cruelty — solving the coordination problem of 'how do we regulate at all without banning use outright.'
% TRANSFER_FUNCTION: Moves the burden of suffering onto animals in ordinary, statutorily-permitted use (confinement, transport, slaughter, research) while reserving legal protection for the narrower category of excess or gratuitous harm; moves reputational and price benefits to producers, certifiers, and consumers who can point to the welfare framework as evidence of moral adequacy.
% ABSENT_VOICES: The animals themselves have no direct voice in any proceeding; animal advocacy organizations proxy for them but are structurally confined to arguing within categories (cruelty vs. ordinary use) that the welfare reading itself defines, and cannot get a hearing for the claim that the category boundary is the harm.
% DISAPPEARANCE_RATIONALE: If the welfare reading vanished, the property reading would likely fill the vacuum by default (use without even the exemption-bounded protections) unless the abolitionist reading gained legislative traction instead — which direction the world rearranges toward is exactly what is contested between the sibling readings; this reading's disappearance does not obviously produce a better or worse world without specifying what replaces it.
% FOUNDING_PROBLEM: 19th- and 20th-century anti-cruelty movements sought to stop the worst, most visible forms of gratuitous animal suffering (public torture, wanton neglect) without directly confronting the economic centrality of animal use to food and industry — welfare statutes were the achievable compromise.
% FOUNDING_PROBLEM_CORROBORATION: Legislators and industry attest the founding problem (preventing gratuitous cruelty) remains live and is being addressed. Animal advocacy organizations and a substantial body of independent philosophical and empirical literature (documenting the scale of suffering within statutorily 'ordinary' confinement and slaughter) attest that the framework has calcified into a mechanism protecting industrial-scale routine suffering that dwarfs the gratuitous-cruelty cases it was designed for — corroboration exists on both sides, and no source entirely outside all interested parties has adjudicated it.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, contested).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε is authored at 0.45 as instructed by the expected structural delta — moderate-high extraction driven by the exemption structure, not by the anti-cruelty function itself, which is genuinely low-extraction where it applies. Suppression (0.58) reflects that the boundary between 'ordinary use' (exempt) and 'cruelty' (actionable) is actively enforced through case law, agency rulemaking, and industry lobbying against expansion of the cruelty standard — it is not a passive default. Theater ratio (0.42) captures the growing certification/labeling apparatus that performs concern for welfare without proportionally reducing the scale of animals held within the exempt category. Accessibility collapse is moderate (0.5): once someone examines the doctrine, they see clearly that 'sentience' does not translate into use-limiting rights, but genuine alternatives (veganism, cultivated meat, reduced research reliance) remain visible and growing, so collapse is not near-total. Resistance (0.55) reflects real, organized, growing challenge from advocacy movements — this is not an unopposed constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this looks like rope: a workable, evidence-responsive standard that has steadily improved animal treatment (bigger cages, better stunning, IRB review) relative to a lawless baseline. From the payer seats — especially animals in uncertified ordinary use, whose situation the doctrine defines as non-cruel by category rather than by evidence of reduced suffering — the same structure looks like extraction wearing a coordination costume: welfare law's real achievement is making industrial-scale use legally comfortable, not reducing suffering proportional to the scale of use. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is 'right,' only that the divergence is structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (industry, research institutions, certifiers, consumers) sit toward the low-d end: the constraint's exemption structure subsidizes their continued use of animals at a manageable compliance cost. The two victim groups are differentiated: animals in gratuitous-cruelty cases are the narrow class the reading actually vindicates (still trapped and powerless, but with a legal remedy in principle); animals in uncertified ordinary use are the much larger class the reading's own exemption structure places outside remedy despite the sentience premise applying equally to them. Both groups get high-d, trapped, powerless treatment, but the second group's exclusion from remedy is the exemption structure's central extractive mechanism — it is who the welfare reading protects LESS than its own premises would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping gratuitous, visible cruelty) is not dead — it still occurs and the doctrine still addresses it — but the doctrine's scope has been extended, through certification and 'humane' branding, to cover and legitimate a vastly larger category of routine industrial use that the founding problem never contemplated at its original scale. This is not simple mandatrophy (mandate fully outlived) but scope creep: a live core function wrapped in an expanding shell of legitimation theater. The founding_problem_status is authored as contested rather than dead precisely because both readings have real corroboration and neither fully displaces the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_committer_structure,
    'This constraint is one reading (welfare_reading) of the contested animal_status kernel; the abolitionist_reading and property_reading are separate constraint stories reading the same underlying practice differently. Where exactly does the disagreement between readings live?',
    'The disagreement is located precisely at the definition of the exemption boundary: property_reading denies that sentience generates any independent constraint beyond what owners/regulators choose to grant (the boundary is wherever positive law happens to draw it, with no principled floor); welfare_reading holds sentience grounds a real but bounded constraint (the boundary tracks ''gratuitous vs. ordinary'' harm); abolitionist_reading holds sentience grounds a right against use itself, making the ordinary/gratuitous distinction morally arbitrary (there is no principled floor above zero use). Resolving which reading is correct would require settling whether moral status is graduated by capacity (welfare''s premise) or threshold-triggering (abolitionist''s premise) — an unresolved question in moral philosophy, not an empirical one.',
    'If the graduated-status premise is correct, welfare_reading''s exemption structure is a defensible non-arbitrary line and this constraint is closer to a genuine tangled_rope with real coordination value. If the threshold-triggering premise is correct, welfare_reading''s exemption structure is simply a wider snare than property_reading''s honest object-status framing, because it launders unrestricted use through a sentience vocabulary the abolitionist reading argues that same vocabulary logically prohibits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reading_committer_structure, conceptual, 'Committer-frame note: identifies the located disagreement between the three sibling readings of the animal_status kernel and what a sibling reading would change structurally.').

omega_variable(
    sentience_evidence_and_exemption_calibration,
    'Is the ordinary-use/gratuitous-cruelty boundary calibrated to actual evidence about animal suffering capacity, or is it calibrated to what industrial practice can economically absorb, with the sentience premise invoked post-hoc to justify wherever the boundary already sits?',
    'Compare boundary placement across jurisdictions and time against contemporaneous scientific consensus on animal cognition/pain — if the boundary moves in lockstep with new sentience evidence rather than with industry cost pressure or advocacy campaign success, calibration is evidence-tracking rather than economically captured.',
    'Evidence-tracking calibration supports the welfare reading''s coordination claim (tangled_rope with a real, evolving standard); economically-captured calibration supports treating the sentience vocabulary as legitimation theater over a structure that is functionally closer to the property reading with better public relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_evidence_and_exemption_calibration, empirical, 'Whether the exemption boundary tracks sentience evidence or economic absorption capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(anim_tr_t60, animal_status__welfare_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(anim_be_t60, animal_status__welfare_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(anim_su_t60, animal_status__welfare_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_status kernel, each authored as a separate constraint story with its own stable epsilon per the ε-invariance principle: property_reading (animals as legal objects; lowest claimed extraction from that reading's own premises, since no independent moral standing is claimed to be violated), welfare_reading (this story; ε≈0.45, moderate-high extraction via exemption structures despite a genuine anti-cruelty coordination core), and abolitionist_reading (animals as rights-holders; highest claimed extraction, since by that reading's premises all instrumental use — not merely its excesses — constitutes the harm). The three do not average into one constraint; each is a distinct structural claim about the same underlying practice of human animal use, linked here for contamination/network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
