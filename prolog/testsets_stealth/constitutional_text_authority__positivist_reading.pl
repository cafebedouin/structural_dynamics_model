% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist Rule of Constitutional Validity (Enactment-Pedigree Reading)
 *   domain: legal/jurisprudential/constitutional
 *
 * SUMMARY:
 *   This story authors the positivist reading of the
 *   constitutional_text_authority kernel: constitutional validity turns on
 *   enactment pedigree (formal procedures, institutional source), and moral
 *   content is inadmissible in the validity determination — the law/morality
 *   distinction is maintained. The colloquial label 'constitutional text
 *   authority' covers three structurally distinct claims about where validity
 *   comes from; per the ε-invariance principle this file authors only the
 *   positivist one, with its own ε, beneficiaries, and victims, and links to
 *   the sibling stories (originalist_reading,
 *   living_constitutionalist_reading) via network.affects_constraints. The ε
 *   referent is the standing arrangement under contest: the pedigree rule as
 *   actually administered, including its justiciability machinery — not any
 *   rival arrangement this reading would endorse. Claim/metric independence
 *   is preserved deliberately: the claimed type is tangled_rope, while the
 *   authored metrics describe a moderately extractive, actively maintained
 *   arrangement whose recited formula increasingly diverges from practice;
 *   the engine measures that divergence rather than the claim adjudicating
 *   it.
 *
 * KEY AGENTS:
 *   - judiciary_validity_gatekeepers: agenda-setter (institutional / identity_locked) — administers the pedigree test and the justiciability machinery that excludes moral grounds; absorbs the legitimacy cost of enforcing valid-but-condemned law
 *   - incumbent_officeholders: primary beneficiary (powerful / constrained) — duly enacted acts stand as law regardless of moral objection
 *   - legislative_majorities: beneficiary (powerful / constrained) — enactment settles validity for their statutes
 *   - legal_profession: beneficiary and payer (organized / constrained) — sells the determinacy the test makes possible; funds its defense against moral-reading critique
 *   - subjects_of_enacted_injustice: primary target (powerless / generational / trapped) — bear valid-but-oppressive arrangements with no validity argument available
 *   - moral_principle_claimants: target (moderate / constrained) — dignity and unenumerated-rights claims dismissed at the threshold or forced into procedural translation
 *   - natural_law_jurisprudents: excluded (moderate / civilizational / identity_locked) — the Fuller-Finnis-Dworkin line, structurally inadmissible in holdings
 *   - comparative_constitutional_observers: analytical observer (analytical / global) — compares validity regimes that admit moral content with those that refuse it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.56).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.62).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist Rule of Constitutional Validity (Enactment-Pedigree Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/jurisprudential/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'e8d13394-3086-4dcb-b823-a42d223367e7').
narrative_ontology:cs_kernel_codification('e8d13394-3086-4dcb-b823-a42d223367e7', fixed_text).
narrative_ontology:cs_authority_grounding('e8d13394-3086-4dcb-b823-a42d223367e7', practice).
narrative_ontology:cs_interpretation_layer_present('e8d13394-3086-4dcb-b823-a42d223367e7').
narrative_ontology:cs_reading_relation('e8d13394-3086-4dcb-b823-a42d223367e7', constitutional_text_authority__originalist_reading, influences).
narrative_ontology:cs_reading_relation('e8d13394-3086-4dcb-b823-a42d223367e7', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('e8d13394-3086-4dcb-b823-a42d223367e7', foundational, validity_by_enactment_pedigree).
narrative_ontology:cs_axiom_status(validity_by_enactment_pedigree, holdable).
narrative_ontology:cs_axiom_grounding('e8d13394-3086-4dcb-b823-a42d223367e7', validity_by_enactment_pedigree, conventional).
narrative_ontology:cs_axiom('e8d13394-3086-4dcb-b823-a42d223367e7', foundational, law_morality_conceptual_separability).
narrative_ontology:cs_axiom_status(law_morality_conceptual_separability, holdable).
narrative_ontology:cs_axiom_grounding('e8d13394-3086-4dcb-b823-a42d223367e7', law_morality_conceptual_separability, conventional).
narrative_ontology:cs_reference_frame('e8d13394-3086-4dcb-b823-a42d223367e7', enactment_pedigree_supremacy).
narrative_ontology:cs_drift_state('e8d13394-3086-4dcb-b823-a42d223367e7', contemporary_rights_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e8d13394-3086-4dcb-b823-a42d223367e7', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, incumbent_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, subjects_of_enacted_injustice).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_principle_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, law_morality_separability_thesis).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, procedural_ideal_of_rule_of_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts and judges decide whether enactments pass constitutional muster by checking pedigree: was it enacted through the prescribed procedures by the prescribed institution? They maintain the justiciability doctrines — standing, ripeness, political question — that keep moral-principle arguments out of validity determinations. When they enforce a valid but widely condemned law they absorb the resulting criticism, and their professional self-understanding as appliers of law rather than makers of value is bound to the pedigree discipline; abandoning it would remake their role.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judiciary_validity_gatekeepers, agenda_setter,
    institutional, generational, identity_locked, national).

% Hold authority whose validity the pedigree test certifies. Their acts, once duly enacted, stand as law regardless of the moral objections raised against them, and challenges must be routed into procedural forms they largely control. Exit means leaving office; while in office the test works for them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, incumbent_officeholders, beneficiary,
    powerful, biographical, constrained, national).

% Pass statutes confident that procedural enactment settles their validity; the moral case against their product is heard, if at all, as politics rather than as a validity argument. Their protection lasts as long as their enactments remain on the books.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_majorities, beneficiary,
    powerful, biographical, constrained, national).

% Sells determinacy: clients buy predictions about what counts as valid law, and the pedigree test is what makes the predictions possible. The profession also spends real resources defending the law/morality distinction against natural-law and moral-reading critique in scholarship, judicial appointment fights, and bar politics.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, legal_profession, payer).

% Live under arrangements that are procedurally valid and materially oppressive — the classic case being enslaved people and disenfranchised groups under duly enacted regimes. Their remedy is political mobilization or amendment, both slower and costlier than a validity argument would be, and they cannot exit the jurisdiction whose valid law binds them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, subjects_of_enacted_injustice, payer,
    powerless, generational, trapped, national).

% Bring constitutional claims grounded in moral principle — dignity, equality, unenumerated rights — rather than in enactment pedigree. The validity framework gives such claims no direct purchase; they survive only by being translated into procedural or textual forms, and many are dismissed at the threshold.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_principle_claimants, payer,
    moderate, biographical, constrained, national).

% Scholars and theorists who argue that law's authority is inseparable from its moral merit — the Fuller, Finnis, and Dworkin line. Their arguments are structurally inadmissible in validity determinations, so their project proceeds in books and dissenting opinions rather than in holdings; abandoning the moral-reading project would dissolve the scholarly identity many of them have built their careers on.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_jurisprudents, excluded,
    moderate, civilizational, identity_locked, global).

% Jurists and scholars in other systems — and in international human rights bodies — who watch how validity tests perform across regimes that admit moral content (proportionality, dignity review) and those that refuse it. They collect neither the test's protection nor its costs; they compare outcomes.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, comparative_constitutional_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, incumbent_officeholders).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared, determinate test for what counts as constitutional law, so that courts, legislatures, officials, and citizens can coordinate on legal obligation without relitigating moral foundations in every case; it gives a pluralistic society with deep moral disagreement a common legal order.
% TRANSFER_FUNCTION: Moves interpretive authority from moral reasoners — philosophers, clergy, natural-law theorists, citizens arguing justice — to procedural institutions: enacting bodies and the courts that apply the pedigree test. It also moves the shield of validity to whatever survives enactment, regardless of content, at the expense of those whose objections are moral rather than procedural.
% ABSENT_VOICES: Those whose constitutional claims rest on moral principle rather than text or procedure — historically enslaved people and disenfranchised groups; today unenumerated-rights and dignity claimants — are structurally absent from validity determinations. They are present in scholarship, dissenting opinions, protest, and international human rights bodies, which is where their objections actually get heard.
% DISAPPEARANCE_RATIONALE: If the pedigree rule vanished overnight, every validity question would reopen as a moral question and courts would need a replacement criterion immediately — ratification-era understanding, contemporary moral principles, or natural law. The entire body of doctrine distinguishing valid from invalid law would be re-derived, and the boundary between law and politics would move to whichever sibling reading took the vacated seat.
% FOUNDING_PROBLEM: How can a legal order be determinate and shared in a society with deep, permanent moral disagreement — and how can law be identified as law without first settling whether it is just? The post-war formulation added: how to say that wicked enacted law is still law (and so identifiable, and so resistable as law) without collapsing legal analysis into moral endorsement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: natural-law and moral-reading theorists (Fuller, Dworkin, Finnis) attest the coordination problem is real even while disputing the pedigree solution; the Hart–Fuller debate record and the grudge-case literature document the problem's persistence; comparative constitutional courts that adopted moral-content tests did so to solve the same determinacy-plus-legitimacy problem, not to deny it. No major jurisprudential school claims the problem was never real.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.56 at interval end): the rule excludes an entire class of constitutional arguments and shields enacted arrangements from moral review, but it sits atop a genuine and enormous coordination function — the rule of recognition — so the extraction is real yet bounded by the value of the determinacy it delivers. Suppression (0.62) reflects active maintenance: courts must refuse moral grounds, and as direct exclusion became contested the justiciability apparatus (standing, ripeness, political question) hardened to do the exclusion work — the rising suppression_requirement series tracks that enforcement build-out, which is the enforcement dynamic this story traces. Theater (0.44, rising) tracks the widening gap between the recited formula (pure pedigree, morality irrelevant) and practice, where moral content re-enters under procedural labels — substantive due process, proportionality, dignity review. Accessibility collapse is low-moderate (0.35): the rival readings do not collapse; they persist as live jurisprudential positions, which is precisely why the rule needs active enforcement rather than self-evidence. Resistance (0.5) is the sustained jurisprudential resistance from the Hart–Fuller debate through Dworkin and the natural-law revival. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by the engine through directionality and scope. All three series share one time grid.
 *
 * PERSPECTIVAL GAP:
 *   The gatekeeper seat and the payer seats should compute differently. From the bench, the pedigree rule is the precondition of legal order: without it every case becomes a moral referendum and the judiciary loses its claim to be applying rather than making law. From the moral claimant's seat, the identical structure is the wall on which their claim breaks — the same rule that constitutes judicial role constitutes claimant exclusion. Likewise the officeholder experiences validation while the jurisprudent experiences inadmissibility, despite adjacent standing: officeholders hold enacted power the rule protects; jurisprudents hold only the kind of argument the rule excludes. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent_officeholders, legislative_majorities, legal_profession) derive low directionality — damped or inverted effective extraction, since the rule subsidizes them. Victims (subjects_of_enacted_injustice, moral_principle_claimants) derive high directionality — amplified effective extraction — and the trapped exit of the subjects seat plus the generational horizon over which they bear valid-but-oppressive regimes places them near the full-target end. National spatial scope amplifies modestly. One override is authored: institutional power atoms to d=0.30. The administering judiciary would derive as a near-beneficiary from its agenda-setting role, but it structurally absorbs the constraint's legitimacy costs — the Hart–Fuller burden of applying valid-but-condemned law and defending the exclusion against critique — a genuinely dual position the beneficiary/victim derivation cannot see; 0.30 places the gatekeeper partway toward the target end while remaining net-subsidized. The comparative observers carry the analytical power atom and are untouched by the override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a determinate validity test for a legal order shared amid permanent moral disagreement — is still live, so no mandatrophy is declared. The tangled_rope classification guards against two mislabels. Pure rope would erase the identifiable payers: the moral claimants whose arguments are structurally inadmissible and the subjects of valid-but-oppressive law, plus the enforcement machinery the exclusion requires. Pure snare would erase the genuine coordination function that even the rule's fiercest critics presuppose when they argue — Dworkin's critique is conducted inside a legal order the rule of recognition holds together. The theater drift is the monitored degradation signal: if practice decouples fully from the recited pedigree test, the rule persists as recitation while justiciability machinery does the real work, and the arrangement drifts piton-ward — maintained by professional habit and the prohibitive cost of re-deriving validity doctrine (no alternative rule of recognition is costlessly available, hence fixing_cost: prohibitive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_source_of_validity,
    'This constraint is the positivist reading of the constitutional_text_authority kernel: would adopting a sibling reading — originalist (validity moored to ratification-era public understanding) or living constitutionalist (authority from contemporary moral principles) — change the beneficiary/victim structure, and how?',
    'Author and compile the sibling stories and compare their structural data: victim sets, enforcement machinery, and directionality derivations under each reading''s validity test.',
    'Under the living constitutionalist reading, moral-principle claimants gain admissibility and this reading''s primary victim seat largely dissolves; under the originalist reading, both moral claimants and living power-holders face a historical-fact test neither controls. The extraction profile of ''constitutional text authority'' is reading-relative, not kernel-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_source_of_validity, conceptual, 'Committer structure: which kernel reading is instantiated and what sibling readings would change structurally.').

omega_variable(
    rule_of_recognition_moral_grounding,
    'Does the pedigree rule itself escape moral content, or does its own authority rest on an unacknowledged moral-political choice — officials'' acceptance, the internal point of view, the grundnorm regress?',
    'Jurisprudential analysis of the rule of recognition''s own validity conditions: test whether the choice of pedigree over rival tests can be stated without normative premises.',
    'If the rule''s foundation is itself moral, the law/morality distinction fails at the base, the claimed neutrality is misdescribed, and the constraint''s extraction profile shifts toward shielding an unacknowledged moral settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_of_recognition_moral_grounding, conceptual, 'Whether the constraint''s foundation is morally neutral or covertly normative.').

omega_variable(
    procedural_labels_moral_content,
    'In hard cases, does pure enactment pedigree actually determine outcomes, or does moral content routinely re-enter under procedural labels — substantive due process, proportionality, dignity review, unenumerated-rights doctrine?',
    'Systematic mapping of invalidation grounds in constitutional case law over the interval: classify each holding''s operative test as pedigree-pure or moral-content-bearing.',
    'If moral content routinely re-enters, the recited rule is substantially performative, the measured theater ratio understates the gap, and the operative exclusion of moral claimants is done by justiciability machinery rather than by the pedigree test itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_labels_moral_content, empirical, 'Whether practice matches the recited pedigree-only test.').

omega_variable(
    procedure_content_neutrality,
    'Are the enactment procedures themselves morally neutral, or do they encode substantive moral choices — who counted as enfranchised at founding and amendment, whose interests the amendment difficulty protects?',
    'Historical and doctrinal analysis of the procedures'' distributive effects: whose moral positions are advantaged by the procedures as designed and as maintained.',
    'If procedures encode morality, the claim that validity ignores moral content is false at one remove: the rule shields a frozen moral settlement, and its victims include everyone disadvantaged by the encoded choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_content_neutrality, conceptual, 'Whether the procedures the rule defers to are themselves content-neutral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__positivist_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__positivist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t48, constitutional_text_authority__positivist_reading, theater_ratio, 48, 0.32).
narrative_ontology:measurement_basis(cons_tr_t48, observed).
narrative_ontology:measurement(cons_tr_t64, constitutional_text_authority__positivist_reading, theater_ratio, 64, 0.38).
narrative_ontology:measurement_basis(cons_tr_t64, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_text_authority__positivist_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement_basis(cons_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__positivist_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__positivist_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t48, constitutional_text_authority__positivist_reading, base_extractiveness, 48, 0.5).
narrative_ontology:measurement_basis(cons_be_t48, observed).
narrative_ontology:measurement(cons_be_t64, constitutional_text_authority__positivist_reading, base_extractiveness, 64, 0.53).
narrative_ontology:measurement_basis(cons_be_t64, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_text_authority__positivist_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(cons_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__positivist_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__positivist_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t48, constitutional_text_authority__positivist_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement_basis(cons_su_t48, observed).
narrative_ontology:measurement(cons_su_t64, constitutional_text_authority__positivist_reading, suppression_requirement, 64, 0.58).
narrative_ontology:measurement_basis(cons_su_t64, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_text_authority__positivist_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(cons_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional text authority' decomposes into three structurally distinct readings of one kernel, each with its own ε, beneficiaries, and victims. This story authors the positivist reading (validity from enactment procedure; law/morality distinction maintained). The originalist sibling substitutes a historical-fact test (ratification-era public understanding) for both procedure and morality; the living constitutionalist sibling admits contemporary moral principles as authority. The positivist reading is upstream of the originalist one — originalism presupposes a determinate text whose validity does not depend on moral content — which is why the reading_relations edge to originalist_reading is 'influences' while the edge to living_constitutionalist_reading is 'coexists_with'. No ε is averaged across readings; each sibling story carries its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
