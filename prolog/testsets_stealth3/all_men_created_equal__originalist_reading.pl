% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Founders-Intent Bound on the Equality Clause (Originalist Reading)
 *   domain: constitutional law/political philosophy/american studies
 *
 * SUMMARY:
 *   The originalist reading of the Declaration's equality clause holds that
 *   the clause's protective scope is fixed by the founding generation's own
 *   understanding: 'all men' meant the men the eighteenth-century social
 *   taxonomy counted as civic equals, and later generations may widen the law
 *   only by formal amendment, never by reinterpreting the founding words. The
 *   standing arrangement this file scores is that bounded-equality regime
 *   itself, described with the reading's own historical candor: a clause
 *   proclaiming unchosen equality, administered through a rule tying its
 *   reach to a taxonomic moment, enforced first by the class that wrote it
 *   and later by a judiciary trained in the method. Epsilon is authored for
 *   the bounded regime as it has actually operated across the interval, never
 *   for any successor arrangement a different interpretive tradition would
 *   install. Family bookkeeping: this file is one member of a three-story
 *   decomposition of the kernel 'all men created equal'; the sibling files
 *   carry different epsilon values because each reading constitutes a
 *   different constraint, not a different view of one constraint. See
 *   network.dual_formulation_note and the kernel_reading_instantiation omega.
 *   KEY AGENTS (by structural relationship): -
 *   founding_era_propertied_white_men: Primary agenda-setter and first
 *   beneficiary (powerful/arbitrage) - authored the clause and the taxonomy
 *   bounding it, retained amendment power -
 *   descendants_within_founding_civic_class: Standing beneficiary
 *   (powerful/constrained) - inherits concentrated civic standing and the
 *   heritage narrative attached to it - enslaved_africans_and_descendants:
 *   Primary target (powerless/trapped) - bore appropriation under statutes
 *   the reading declines to test against the clause -
 *   women_denied_civic_personhood: Target (powerless/identity_locked) -
 *   excluded via coverture and franchise rules; exit fused with family
 *   structure - indigenous_nations: Target (organized/trapped) - treaty
 *   polities subjected to removal and jurisdiction-stripping under the
 *   intent-fixed frame - originalist_judiciary_and_scholars: Modern
 *   agenda-setter (institutional/constrained) - administers the method,
 *   careers invested in its authority -
 *   post_ratification_unconsenting_generations: Excluded seat
 *   (powerless/trapped) - born bound by a scope rule set without them -
 *   comparative_constitutional_historians: Analytical observer
 *   (analytical/analytical) - holds the archival record all seats argue from
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.62).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Founders-Intent Bound on the Equality Clause (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional law/political philosophy/american studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '4795e3bc-d8a1-415e-85c5-8ba8a477b6b6').
narrative_ontology:cs_kernel_codification('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', fixed_text).
narrative_ontology:cs_authority_grounding('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', lineage).
narrative_ontology:cs_interpretation_layer_present('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6').
narrative_ontology:cs_reading_relation('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', foundational, founder_intent_fixes_equality_scope).
narrative_ontology:cs_axiom_status(founder_intent_fixes_equality_scope, holdable).
narrative_ontology:cs_axiom_grounding('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', founder_intent_fixes_equality_scope, conventional).
narrative_ontology:cs_axiom('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', secondary, civic_equality_bounded_by_founding_membership_class).
narrative_ontology:cs_axiom_status(civic_equality_bounded_by_founding_membership_class, holdable).
narrative_ontology:cs_axiom_grounding('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', civic_equality_bounded_by_founding_membership_class, conventional).
narrative_ontology:cs_reference_frame('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', founding_civic_taxonomy_scope).
narrative_ontology:cs_drift_state('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', contemporary_rights_revolution_aftermath, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4795e3bc-d8a1-415e-85c5-8ba8a477b6b6', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_era_propertied_white_men).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, descendants_within_founding_civic_class).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_denied_civic_personhood).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, founding_consent_popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, strict_construction_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Declaration's equality language and the state and federal instruments that gave it legal force, while writing the property qualifications, slave codes, and coverture rules that fixed who counted as a civic equal. They chose the clause's words with full knowledge of the plantation economies and household hierarchies surrounding them, and kept the power to amend or discard what they wrote. Exit was effectively unlimited: they could reshape, reinterpret, or abandon the arrangement at will, and many drew income and standing directly from maintaining it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_era_propertied_white_men, agenda_setter,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, founding_era_propertied_white_men, beneficiary).

% Inherited the civic standing, accumulated property, and institutional control that the founding settlement concentrated in the founding class, along with a heritage narrative tying family and regional identity to the founders' authority. Formal barriers fell across the nineteenth and twentieth centuries, but inherited wealth gaps, school and neighborhood boundaries, and presumptive cultural authority continue to flow from the original bounding of the equality promise. Leaving the position would mean renouncing inherited advantage and the ancestral story that frames it, which few households or institutions attempt.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, descendants_within_founding_civic_class, beneficiary,
    powerful, generational, constrained, national).

% Were held as chattel property under statutes written by the same assemblies that ratified the equality language; their labor, families, and bodies were appropriated under law that the founders-intent rule refuses to measure against the clause's words. Flight meant pursuit under fugitive-slave acts, revolt meant executions, and petitioning invoked a document whose authorized interpreters denied its application to them. Descendants entered freedom into a century of disenfranchisement, convict leasing, and exclusion from the land-grant and veterans' programs that built middle-class wealth elsewhere.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants, payer,
    powerless, biographical, trapped, national).

% Were placed outside the civic class by coverture and franchise exclusion written into the founding settlements, denied independent property, contract, and vote under the same intent-governed reading that held the clause inapplicable to them. Exit ran through the family itself: economic survival, legal identity, and social standing were fused with marriage and household roles, and the ideology of separate spheres made the exclusion feel like nature rather than decree. Organization eventually arrived through voluntary associations that had to argue their way into a conversation the clause's authorized readers controlled.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_denied_civic_personhood, payer,
    powerless, biographical, identity_locked, national).

% Entered the founding era as organized diplomacies holding their own treaties, and watched the equality language be invoked to justify removal, allotment, and boarding-school assimilation while their polities were stripped of jurisdiction. Their power was real but diplomatic and military rather than constitutional: the intent-fixed frame treated sovereignty as a domestic matter already settled by the founders' wars and purchases. Exit meant forced relocation along removal routes or confinement to reservation borders that settler governments redrew at will.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    organized, generational, trapped, continental).

% Administer the founders-intent rule today: judges resolve equal-protection and due-process questions by consulting founding-era meaning, and the academic bar certifies which historical sources count. Careers, reputations, and doctrinal legacies are invested in the method's authority; breaking from it mid-career carries real professional cost, while defending it yields clerkships, chairs, and appointments. They did not write the original bounding, but they decide case by case how much of it still governs.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judiciary_and_scholars, agenda_setter,
    institutional, generational, constrained, national).

% Are born into a polity whose equality commitments were scoped by men two centuries dead, under a rule that treats the founding generation's understanding as controlling regardless of present consent. They inherit the scope decision without having been party to it and without a procedural route to revisit it short of amendment supermajorities; their objection is structural rather than episodic, and no seat exists for it in the interpretive conversation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, post_ratification_unconsenting_generations, excluded,
    powerless, generational, trapped, national).

% Study the clause's drafting, ratification, and invocation across jurisdictions and periods, comparing the American intent-fixed treatment with other founding documents' trajectories. They publish the archival record on which every seat's claims depend, take no part in enforcement, and sit outside the professional incentive structure of the American method schools.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, comparative_constitutional_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, descendants_within_founding_civic_class).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the rule of recognition for equality claims: disputes about who the clause protects resolve by consulting founding-era understanding rather than open-ended moral argument, giving officials, courts, and citizens a single stable anchor, constraining judicial discretion, and channeling scope revisions through the amendment process the founders built.
% TRANSFER_FUNCTION: Moves civic standing, legal protection, and custody of the founding language's moral authority from the excluded classes (enslaved people, women, Indigenous nations, and their descendants) to the founding civic class and its heirs; moves authority over the clause's meaning from living citizens to the founding generation's recorded understanding.
% ABSENT_VOICES: At the drafting and ratifying tables the clause's excluded subjects had no seat: the enslaved, whose objections survive in fragments (Cugoano and Equiano in print, the 1770s-80s northern emancipation petitions invoking the clause); women (Abigail Adams's 'remember the ladies' letter answered with laughter); and Indigenous diplomacies, addressed by treaty and rifle rather than by the clause. Radical abolitionists were later read out of the founding consensus as fanatics, and unconsenting future generations have no procedural seat at all under a rule that fixes scope at the founders' understanding.
% DISAPPEARANCE_RATIONALE: Overnight deletion of the intent-fixed scope rule would reorganize American equality jurisprudence around the surviving interpretive traditions within a decade: equal-protection analysis would lose its historical anchor, remedy politics (race-conscious admissions, voting-rights designations, reparations arguments) would shift ground immediately, the descendant civic class would lose a legitimating title it currently cites, and the interpretive profession would redistribute authority toward whichever successor method consolidated first. The physical world barely notices; the legal-political order rearranges substantially.
% FOUNDING_PROBLEM: Legitimating a new union: thirteen former colonies needed a shared political identity and a standard against which British rule could be condemned, and the equality clause supplied the anti-monarchical banner - but ratification required holding a coalition whose southern members depended on chattel slavery and whose members everywhere assumed racial and household hierarchy, so the clause's universality had to be proclaimed while its operative scope was bounded to the founding civic class.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the benefiting order exists for the problem itself: the 1780 Pennsylvania Gradual Abolition Act preamble, the northern emancipation petitions of the 1770s-80s, and Lincoln's 1857 Springfield speech all attest that the founders launched the clause knowing its scope was contested and meant it as a reproach or a standard - the founding problem (unify while containing the slavery contradiction) is independently documented. On STATUS, no neutral witness favors the originalist answer: the strongest authorities for a permanently closed taxonomy are the Dred Scott opinion and the proslavery exegetes, all inside the benefiting order, while historians outside it (Wills, Waldstreicher, Rakove) attest the opposite - that founding-era intent on scope was divided and unsettled from the start. Stated plainly: the reading's own genealogy claim lacks any corroborator outside its beneficiary set.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high across most of the interval (peak 0.84 at 1860; 0.62 at the 2024 endpoint) because the reading's operation moved labor, civic standing, and the moral authority of the founding language from the excluded classes to the founding class and its heirs, and because intent-fixed colorblindness continues to strip corrective measures from descendant seats in contemporary remedy politics. Suppression is a raw structural property and is NOT scaled by power or scope; the scalar reports the endpoint enforcement picture (0.38, judicial-administrative) while the suppression_requirement series traces the enforcement-capacity arc: heavy coercion at the slave-code and Redemption phases (0.80, 0.78), lighter formal enforcement today. Theater runs high at the origin (proclaiming universal equality while ratifying slave codes, 0.62 at 1776), spikes again under 'separate but equal' (0.63 at 1896), and settles moderate. Resistance is high (0.72): abolition coalitions, the Civil War, suffrage organization, and the civil-rights movement confronted the reading directly; the enforcement machinery specifically targeted coalition infrastructure (gag rules, mail bans, fugitive-slave prosecution of allies), which is the signature of a constructed arrangement defended against its subjects rather than a regularity nobody contests. Alternatives never fully collapsed (accessibility_collapse 0.62): counter-readings survived underground, abroad, and in dissents throughout. The series is cyclical rather than monotonic - rise, crisis, reconstruction concession, redemption retrenchment, rights-revolution breakthrough, methodist resurgence - and the oscillation itself functioned as intermittent reinforcement, teaching excluded seats that concessions were provisional. The base_properties scalars reflect the 2024 endpoint state. Claimed type is tangled_rope: a genuine interpretive-stability function (a determinate rule of recognition constraining judicial discretion, revisable through the amendment process the founders built) coexists with asymmetric extraction through the same structure, actively enforced. The claim and the metrics are independent authored facts; where the engine's per-seat computation diverges from this claim, that divergence is the datum the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the founding-class and descendant seats the reading is inheritance: a legitimate title to standing, experienced as continuity rather than imposition. From the trapped payer seats the same rule is the wall itself: the clause's promise exists audibly and is withheld by a scope decision those seats never made. The judiciary seat splits internally - administering the method feels like neutral craft to its practitioners while functioning as enforcement for the benefiting order; its derived directionality sits nearer the beneficiary pole than its self-description admits, hence the override. Same-level comparison among the payer seats shows exit differentiation doing the structural work: enslaved people faced trapped exit (flight punishable, body owned), women faced identity-locked exit (economic and legal existence fused with the household), Indigenous nations held organized power but trapped territory - comparable exclusion, three different cages, three different coalition paths. Coalition capacity mattered: the eventual abolition coalition combined the powerless payer class with free Black communities, radical presses, and defecting elites, which is why the enforcement machinery invested so heavily in severing exactly those alliances.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as real actors: the founding propertied class (wrote the bound, collected immediately, held arbitrage-grade exit over rules of its own making) and the descendant civic class (collects durably through inherited standing and institutional control; exit constrained because leaving means renouncing inheritance and the ancestral narrative fused with it - a mild identity lock on the receiving side). Victims are declared as the classes whose standing the bound withholds: enslaved people and descendants, women, and Indigenous nations, each near the target pole, amplified by trapped or identity-locked exits. The judiciary-and-scholarship seat is overridden from its derived near-beneficiary value to 0.32: it collects career, clerking, and doctrinal rents from the method, but it is also partially captured by the method's prestige economy, bearing costs (professional risk, the scholarly burden of archival defense) the structural derivation cannot see. The vindicated propositions (founding-consent popular sovereignty, strict-construction legitimacy) are listed separately; they collect no rents and are not beneficiaries. Larger spatial scope (continental at founding) raised verification difficulty historically; the national-scale modern operation verifies more easily and scales effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - welding thirteen ex-colonies into a legitimated union without shattering the slaveholding coalition, using anti-monarchical equality language as the common banner - is scored contested: the union-building purpose is spent on any account, yet the scope rule persists with a live administrative apparatus, which is the classic shape of a mandate outliving its function. Declaring the arrangement a tangled rope rather than a pure extraction mechanism preserves the genuine coordination value that makes the reading attractive even to non-extractors (interpretive determinacy constraining discretion); declaring it a pure coordination mechanism would erase the victim declarations and launder the transfer. The R5 interview locates the zombie risk precisely: a scope rule built for 1770s coalition management now governs twenty-first-century remedy politics, administered by professionals whose incentives favor persistence. The mismatch consumer should watch the (founding_problem_status x disappearance_verdict) cell here. If the dead-hand premise breaks (see omega dead_hand_authority), the reading converts from governing method to inertial remainder - a degraded trajectory - and classification should follow the break, not the label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This file instantiates only the originalist reading of the kernel ''all men are created equal''; would the universalist or textualist-paradox readings of the same clause classify differently, and on which structural element does the disagreement turn?',
    'Read the two sibling stories in the family (linked via network.affects_constraints) and compare victim sets, epsilon, and claimed types. The disagreement is located in the scope-determination rule itself (founder intent versus iterative universal application versus performative-contradiction diagnosis), not in any empirical fact about the founding record.',
    'Under the universalist sibling the excluded-group victim set shrinks toward historical-only and epsilon falls; under the textualist-paradox sibling the same arrangement is scored as self-undermining rather than stable, shifting classification toward decay dynamics. This file''s values bind only to the originalist instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one reading of the all_men_created_equal kernel; siblings are separate constraints.').

omega_variable(
    intent_recoverability,
    'Is a determinate founders'' intent on the clause''s scope recoverable from the archival record, or is ''founders'' intent'' assembled selectively after the fact?',
    'Systematic coding of ratification-era statements on the clause''s application (state ratifying conventions, Federalist and Antifederalist exchanges, early state constitutions'' own equality provisions) against the scope rule later attributed to them.',
    'If intent is indeterminate, the reading''s coordination value (a stable rule of recognition) collapses into selective citation and the arrangement trends toward theatrical maintenance of a rule nobody can state; if determinate, the coordination function stands and the extraction verdict rests on scope alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_recoverability, empirical, 'Whether the reading''s anchor (founders'' intent) exists as a determinate object.').

omega_variable(
    entitlement_baseline_dispute,
    'Does bounding the clause take anything OWED to the excluded (making the operation extraction with identifiable victims), or merely withhold a protection never promised (making it a scope limit with no victim)?',
    'Settle the baseline: if the clause''s own natural-rights grammar (equality of persons prior to any civic taxonomy) generates an entitlement independent of founder understanding, the bound removes something owed and victims exist; if entitlement is wholly constituted by the founders'' civic taxonomy, the bound withholds only. The textualist-paradox sibling argues the clause''s universal grammar itself refutes the second horn.',
    'On the no-baseline horn epsilon drops toward the coordination-cost floor and the reading approaches a pure interpretive convention; on the owed-baseline horn epsilon rises and the victim declarations harden. Per-seat classification swings between coordination-side and extraction-side on this single unresolved premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entitlement_baseline_dispute, conceptual, 'Baseline question determining whether the reading has victims at all.').

omega_variable(
    dead_hand_authority,
    'May the founding generation''s understanding legitimately bind the equality commitments of the living, who never consented to the scope rule?',
    'Comparative constitutional analysis of other founding documents'' reinterpretation norms, plus the revealed-preference record of amendment frequency actually used to revise equality scope (the Reconstruction Amendments) as evidence of how binding the polity itself treats the founding scope.',
    'If dead-hand authority is rejected, the reading loses its legitimacy ground and survives mainly as inertia, converting the classification toward degraded-inertial dynamics; if accepted, the reading is a live governing method and the transfer it effects is a chosen policy rather than a residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dead_hand_authority, preference, 'Intergenerational authority premise beneath the reading.').

omega_variable(
    post_amendment_identity,
    'After the Reconstruction Amendments rewrote the operative text, is the founders-intent reading of the amended Fourteenth Amendment the SAME constraint scored across this interval, or a successor constraint sharing the label?',
    'Test epsilon invariance across the amendment boundary: if the reading''s victim set, enforcement mode, and epsilon change discontinuously at 1865-1870, decompose the family into a pre-amendment and a post-amendment story joined by network edges.',
    'If decomposed, the pre-amendment story carries nearly all historical extraction (epsilon peaking at 0.84) and the post-amendment story is a thinner methodological rule over remedial politics; keeping one story blends two epsilons under one label, the exact failure the decomposition discipline exists to prevent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_amendment_identity, conceptual, 'Epsilon-invariance check across the Reconstruction amendment boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.62).
narrative_ontology:measurement_basis(all__tr_t1776, observed).
narrative_ontology:measurement(all__tr_t1808, all_men_created_equal__originalist_reading, theater_ratio, 1808, 0.55).
narrative_ontology:measurement_basis(all__tr_t1808, observed).
narrative_ontology:measurement(all__tr_t1840, all_men_created_equal__originalist_reading, theater_ratio, 1840, 0.52).
narrative_ontology:measurement_basis(all__tr_t1840, observed).
narrative_ontology:measurement(all__tr_t1860, all_men_created_equal__originalist_reading, theater_ratio, 1860, 0.5).
narrative_ontology:measurement_basis(all__tr_t1860, observed).
narrative_ontology:measurement(all__tr_t1877, all_men_created_equal__originalist_reading, theater_ratio, 1877, 0.58).
narrative_ontology:measurement_basis(all__tr_t1877, observed).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__originalist_reading, theater_ratio, 1896, 0.63).
narrative_ontology:measurement_basis(all__tr_t1896, observed).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.44).
narrative_ontology:measurement_basis(all__tr_t1954, observed).
narrative_ontology:measurement(all__tr_t1978, all_men_created_equal__originalist_reading, theater_ratio, 1978, 0.38).
narrative_ontology:measurement_basis(all__tr_t1978, observed).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(all__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.55).
narrative_ontology:measurement_basis(all__be_t1776, observed).
narrative_ontology:measurement(all__be_t1808, all_men_created_equal__originalist_reading, base_extractiveness, 1808, 0.66).
narrative_ontology:measurement_basis(all__be_t1808, observed).
narrative_ontology:measurement(all__be_t1840, all_men_created_equal__originalist_reading, base_extractiveness, 1840, 0.74).
narrative_ontology:measurement_basis(all__be_t1840, observed).
narrative_ontology:measurement(all__be_t1860, all_men_created_equal__originalist_reading, base_extractiveness, 1860, 0.84).
narrative_ontology:measurement_basis(all__be_t1860, observed).
narrative_ontology:measurement(all__be_t1877, all_men_created_equal__originalist_reading, base_extractiveness, 1877, 0.64).
narrative_ontology:measurement_basis(all__be_t1877, observed).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__originalist_reading, base_extractiveness, 1896, 0.72).
narrative_ontology:measurement_basis(all__be_t1896, observed).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.46).
narrative_ontology:measurement_basis(all__be_t1954, observed).
narrative_ontology:measurement(all__be_t1978, all_men_created_equal__originalist_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement_basis(all__be_t1978, observed).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(all__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.4).
narrative_ontology:measurement_basis(all__su_t1776, observed).
narrative_ontology:measurement(all__su_t1808, all_men_created_equal__originalist_reading, suppression_requirement, 1808, 0.58).
narrative_ontology:measurement_basis(all__su_t1808, observed).
narrative_ontology:measurement(all__su_t1840, all_men_created_equal__originalist_reading, suppression_requirement, 1840, 0.68).
narrative_ontology:measurement_basis(all__su_t1840, observed).
narrative_ontology:measurement(all__su_t1860, all_men_created_equal__originalist_reading, suppression_requirement, 1860, 0.8).
narrative_ontology:measurement_basis(all__su_t1860, observed).
narrative_ontology:measurement(all__su_t1877, all_men_created_equal__originalist_reading, suppression_requirement, 1877, 0.74).
narrative_ontology:measurement_basis(all__su_t1877, observed).
narrative_ontology:measurement(all__su_t1896, all_men_created_equal__originalist_reading, suppression_requirement, 1896, 0.78).
narrative_ontology:measurement_basis(all__su_t1896, observed).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.52).
narrative_ontology:measurement_basis(all__su_t1954, observed).
narrative_ontology:measurement(all__su_t1978, all_men_created_equal__originalist_reading, suppression_requirement, 1978, 0.34).
narrative_ontology:measurement_basis(all__su_t1978, observed).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(all__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% 'Equality bounded by founders' intent' is one reading of the kernel 'all men are created equal', not the kernel itself. The family decomposes on the scope-determination rule: originalist (intent-fixed), universalist (iteratively expanding), textualist-paradox (self-undermining gap). Epsilon values differ across the family because each reading constitutes a different constraint with a different victim set and enforcement profile; this file's epsilon (0.62 endpoint, 0.84 historical peak) binds only to the originalist instantiation. Edges run from this reading to the universalist sibling (coexists_with) and to the textualist-paradox sibling (influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
