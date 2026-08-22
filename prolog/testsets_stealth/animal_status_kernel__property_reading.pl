% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property-Status Frame (Ownership Reading)
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This story instantiates the property reading of the animal-status kernel
 *   as a clean, single-epsilon constraint: animals are chattels, moral
 *   considerability derives from ownership rights, and economic value is the
 *   only value the arrangement must reckon. The referent of every metric is
 *   the standing arrangement under contest — animals held and used as
 *   property, with anti-cruelty statutes applied by courts through the
 *   owner's property interest rather than the animal's — assessed by this
 *   reading's own lights. The reading's normative commitments live in the
 *   structure it declares: a three-seat beneficiary set (owners, industrial
 *   operators, consumers), a victim-set deliberately left empty because under
 *   this reading animals are objects of title rather than parties, and a
 *   claimed type of rope — the reading's self-understanding of property
 *   rights as a coordination device for asset ownership, transfer, and
 *   investment security. The authored metrics are independent of that claim:
 *   the arrangement allocates the whole of animal life (labor, products,
 *   offspring, lives) to owners with no countervailing moral constraint on
 *   use (extractiveness 0.88), holds against contest through standing
 *   denials, ag-gag statutes, and preemption (suppression 0.62), and
 *   increasingly performs protection through an anti-cruelty layer that
 *   operates as property-value protection (theater 0.42). This file is one
 *   member of a three-story family decomposing the kernel; the siblings and
 *   the location of the disagreement are recorded in kernel_context and the
 *   network note.
 *
 * KEY AGENTS:
 *   - animal_owners: primary beneficiary (powerful/mobile) — hold title; the arrangement's value accrues to them as an incident of ownership
 *   - industrial_livestock_operators: agenda-setter and concentrated beneficiary (institutional/arbitrage) — operate animal use at scale, shape the statutes that secure it, and receive the bulk of what animals produce
 *   - animal_product_consumers: secondary beneficiary (moderate/mobile) — buy animal products priced as if the animals' own costs were zero
 *   - owned_animals: excluded seat (powerless/trapped) — the objects of every rule and parties to none; no forum recognizes them as claim-holders
 *   - animal_advocacy_organizations: excluded seat (organized/constrained) — litigate and investigate from inside the frame that denies their suits standing
 *   - legislatures_and_courts: agenda-setter (institutional/analytical) — maintain the frame through property doctrine, standing doctrine, and preemption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.62).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, rope).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property-Status Frame (Ownership Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4').
narrative_ontology:cs_kernel_codification('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', formalized).
narrative_ontology:cs_authority_grounding('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', lineage).
narrative_ontology:cs_interpretation_layer_present('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4').
narrative_ontology:cs_reading_relation('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', animal_status_kernel__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', foundational, economic_value_exclusive_relevance).
narrative_ontology:cs_axiom_status(economic_value_exclusive_relevance, holdable).
narrative_ontology:cs_axiom_grounding('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', economic_value_exclusive_relevance, conventional).
narrative_ontology:cs_axiom('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', foundational, ownership_grounds_considerability).
narrative_ontology:cs_axiom_status(ownership_grounds_considerability, holdable).
narrative_ontology:cs_axiom_grounding('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', ownership_grounds_considerability, conventional).
narrative_ontology:cs_axiom('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', secondary, anti_cruelty_protects_owner_property_value).
narrative_ontology:cs_axiom_status(anti_cruelty_protects_owner_property_value, holdable).
narrative_ontology:cs_axiom_grounding('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', anti_cruelty_protects_owner_property_value, conventional).
narrative_ontology:cs_reference_frame('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', chattel_property_baseline).
narrative_ontology:cs_drift_state('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', contemporary_animal_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a19d746d-00db-4ea2-a9cb-1d3bfcc09fd4', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, industrial_livestock_operators).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, chattel_property_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, customary_practice_safe_harbor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals as a class of asset. May buy, sell, breed, confine, transport, and kill the animals they own, subject only to anti-cruelty statutes that courts apply through the owner's property interest. The full economic value their animals produce — labor, products, offspring — accrues to them as an incident of title. Exit is ordinary asset disposal: an owner who no longer wants the arrangement sells the animals or leaves animal husbandry and bears nothing beyond the transaction.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, beneficiary,
    powerful, biographical, mobile, global).

% Operate the largest share of animal use at industrial scale and shape the legal frame that secures it: lobbying for right-to-farm statutes, ag-gag statutes, customary-farming exemptions that fold standard industry practice into the anti-cruelty safe harbor, and preemption of local welfare measures. Collect the bulk of the value animals produce. Can relocate operations across state and national lines when any jurisdiction tightens its rules, so no single jurisdiction's law binds them for long.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, industrial_livestock_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, industrial_livestock_operators, beneficiary).

% Buy animal products at prices set by production systems in which the animals' own costs are priced at zero. Individual exit — substituting plant-based alternatives — is easy and increasingly available, but demand is diffuse and no consumer directly bears or checks the conditions under which the products are made.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_product_consumers, beneficiary,
    moderate, immediate, mobile, global).

% Are the objects of every rule in the arrangement and parties to none of them. Everything the arrangement allocates — labor, products, offspring, lives — flows from them to their owners; what flows back is feed, housing, and handling calibrated to asset value. They cannot exit, refuse, or appeal: no forum recognizes them as capable of holding a claim, and the frame treats them as objects of title rather than participants. Their seat is recorded because the arrangement's entire operation runs through them.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, owned_animals, excluded,
    powerless, biographical, trapped, global).

% Litigate, legislate, and investigate to move animals out of the bare-asset category. Courts routinely dismiss their suits for lack of a cognizable interest — the animals they seek to represent are owned by the defendants — and ag-gag statutes criminalize the undercover recording on which most exposure of standard practice depends. They operate entirely inside jurisdictions whose property law they are trying to change, so exit would mean abandoning the work.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Maintain the legal frame: property and commercial law define animals as chattels; courts apply anti-cruelty statutes through the owner's property interest, decline standing for animals, and read unnecessary-suffering standards against customary practice; legislatures enact preemption and ag-gag measures when local majorities move first. They administer the arrangement without holding title to the assets it allocates.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, industrial_livestock_operators).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the asset-control problem for a class of economically valuable living property: who may use an animal, who captures its products and offspring, who bears its costs, and how title transfers. A uniform answer by ownership gives holders the security to invest in breeding, housing, training, and husbandry, and gives markets a fungible asset class.
% TRANSFER_FUNCTION: Moves the whole of each animal's economic and bodily value — labor, products, offspring, and life itself — from the animal to its owner, and moves would-be moral intervention out of the category of legal constraint on use and into the category of a market signal priced at the margin.
% ABSENT_VOICES: The animals are absent from every forum where their status is set: they are the objects of the rules and parties to none, and this reading holds they could not be parties — which is exactly what the sibling readings deny. Animal advocates are present in public discourse but absent where decision binds: courts dismiss their suits for lack of a cognizable interest, and ag-gag statutes remove the evidence-gathering that would ground claims. Veterinarians and line workers who witness standard practice face employment and reporting pressures that keep them largely out of the conversation.
% DISAPPEARANCE_RATIONALE: Title to billions of animals, capital embedded in confinement, processing, and laboratory infrastructure, biomedical research and pharmaceutical testing protocols, livestock-theft and pet-custody law, and the price structure of animal products all presuppose the frame. Overnight removal would strand assets, void contracts, and force legislatures and courts to rebuild allocation rules from first principles — the world rearranges.
% FOUNDING_PROBLEM: Securing exclusive, transferable control over animals as productive assets: preventing theft, resolving use disputes, and giving owners the security to invest in animals whose value accrues only if title holds.
% FOUNDING_PROBLEM_CORROBORATION: Commercial-law and legal historians, writing outside the beneficiary set, attest that animal-title and livestock-theft rules arose to secure exclusive, transferable control of valuable assets in agrarian and mercantile economies, and the independent emergence of such rules across unrelated legal systems corroborates the coordination problem's reality. Animal advocacy organizations — also outside the beneficiary set — concede the allocation problem exists while disputing that it requires holding animals as bare assets. No corroborating source claims that the moral-exclusion features (considerability via ownership, economic value as the only value) were part of the founding problem; both appear in the record as later justifications layered onto the title rules.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.88: the reading's own structural delta expects high extraction precisely because nothing in this constraint counts against use — no interest of the used party enters the calculus, so the arrangement takes the entire economic and bodily value of animals. The reading authors that fact honestly while denying its wrongful character: the denial is encoded in the empty victim-set and the rope claim, not in a discounted epsilon. Suppression 0.62: the frame rarely needs daily coercion because it is constitutive of ordinary commerce, but it actively enforces against contest — courts dismiss advocacy suits for lack of cognizable interest, ag-gag statutes criminalize investigation, preemption voids local welfare measures. Suppression is authored as a raw structural property and is not scaled here; the engine, not this story, scales extraction by directionality and scope. Theater 0.42 and rising: as public concern grew, the anti-cruelty layer became the frame's public face while customary-farming exemptions folded standard industry practice into the safe harbor — protection performed, property value protected. Accessibility collapse 0.6: the personhood alternative is foreclosed in law, while the welfare alternative persists but is absorbed through the property frame's interpretive layer. Resistance 0.5: a substantial advocacy movement operates inside the frame it seeks to change. All three measurement series share one time grid (decades 0-60) so every metric is authored at every examined point; trajectories are monotonic — industrial intensification drives extraction, humane-washing drives theater, and enforcement machinery expands as contest grows.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the owner and operator seats the arrangement is the ordinary incident of ownership — title, transfer, investment security — and should compute as low-extraction coordination; the operator seat adds agenda-setting power and arbitrage exit, the strongest beneficiary position in the story. From the animals' seat — powerless, trapped, overridden to the full-target end of directionality — the same structure computes as near-total taking with no exit, no voice, and no forum. The advocate seat sits between: it bears the frame's enforcement costs (dismissed suits, ag-gag exposure) while collecting none of its value. The claimed rope is the reading's own self-assessment from inside the frame; it does not adjudicate these seats, and the divergence between that claim and the computed animal seat is the datum this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: animal_owners (mobile exit), industrial_livestock_operators (arbitrage), and animal_product_consumers (mobile) all derive low directionality — the arrangement subsidizes them, the operators most concentratedly, which is why gain_flow names that seat. The reading declares no victims, by construction, so the animals' seat would otherwise fall to a power-atom canonical fallback; the override sets the powerless atom to 0.97 because the structural fact is unambiguous — the arrangement takes everything from them and they cannot exit — and the victim-set exclusion is a normative act of this reading, not a structural finding about who bears the arrangement's operation. The organized-atom override (0.6) covers the advocacy seat: advocates bear the frame's enforcement costs without collecting any of its value, but their costs are defensive (litigation, investigation exposure) rather than extracted value, so they sit moderately target-side rather than at the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing exclusive, transferable control over animals as productive assets — is still live: animals remain economically valuable assets in every jurisdiction and title rules still resolve real disputes, so no mandatrophy is declared and the founding_problem_status is live with corroboration from outside the beneficiary set. The misclassification risk in this family runs in the direction the rope claim itself illustrates: a genuine coordination function (title, transfer, investment security) can serve as the cover under which an arrangement whose dominant operation is the unconditional taking of animal life persists. The classification apparatus handles this by refusing to let the coordination story settle the question — the per-seat computation prices each seat's position from power, exit, and directionality, and the animals' seat computes from the structural override rather than from the reading's victim-set denial. If animals ever cease to be assets, the founding problem dies and the frame would persist by inertia and performance; this story should then be re-authored toward a piton reading with theater_ratio as the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the property_reading of the animal_status kernel; how would the welfare_reading or abolitionist_reading change the constraint''s victim-set, extractiveness, and classification if instantiated instead?',
    'Author and compare the sibling stories over the same referent: the welfare_reading adds animals to the victim-set and damps extractiveness through welfare obligations it treats as genuine constraints; the abolitionist_reading names property status itself as the injury, maximizes extractiveness, and claims a snare. The disagreement is located in two structural elements: victim-set membership, and whether any non-economic value constrains use.',
    'Under the welfare reading the standing arrangement should compute as a tangled rope (coordination plus asymmetric extraction); under the abolitionist reading as a pure snare. The rope claim and the empty victim-set are constitutive of this file only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the animal-status kernel this constraint is, and what siblings would change.').

omega_variable(
    standing_denial_durability,
    'Will courts continue to deny animals standing, or will personhood-adjacent experiments — habeas petitions, sentient-being constitutional clauses, guardianship statutes — crack the frame within the story''s horizon?',
    'Track outcomes of animal standing litigation and constitutional or statutory reforms across jurisdictions; a single granted-standing precedent carrying remedies would mark the crack.',
    'Granted standing moves animals into the claim-holder set, forces the frame to defend against inside claims rather than outside ones (raising suppression), and shifts the animals'' computed seat from trapped object to constrained party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_denial_durability, empirical, 'Durability of the standing denial that keeps animals outside the victim-set.').

omega_variable(
    anti_cruelty_enforcement_referent,
    'What fraction of anti-cruelty enforcement actually protects animal interests rather than owner property value, and how does the split differ between companion animals and farmed animals under customary-farming exemptions?',
    'Audit prosecution and inspection records: charges brought, exemptions applied, and outcomes measured for the animal versus restitution to the owner.',
    'If farmed-animal enforcement is near zero once exemptions are applied, the theater_ratio is understated and the anti-cruelty layer is almost wholly performance; if companion-animal enforcement is substantial, the layer retains a real function the theater metric should not erase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_cruelty_enforcement_referent, empirical, 'Whether the anti-cruelty layer functions as animal protection or property-value protection.').

omega_variable(
    welfare_overlay_absorption,
    'Are welfare statutes a genuine constraint on the property frame, or are they absorbed as property-value protection — has the standing arrangement drifted toward the welfare sibling''s constraint without changing this reading''s classification?',
    'Compare welfare-statute outcomes against what the welfare reading would predict: binding rules should produce measured confinement and slaughter-line reductions that track animal interests; absorbed rules should track industry cost curves.',
    'Genuine constraint would date this reading''s extractiveness high-water mark in the past and push the arrangement toward the welfare sibling''s file; absorption confirms the property reading as still operative and the theater series as the right trace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_overlay_absorption, conceptual, 'Whether welfare overlays constrain the frame or are absorbed by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_property_reading_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(animal_property_reading_tr_t10, animal_status_kernel__property_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(animal_property_reading_tr_t20, animal_status_kernel__property_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(animal_property_reading_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(animal_property_reading_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(animal_property_reading_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(animal_property_reading_tr_t60, animal_status_kernel__property_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(animal_property_reading_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement(animal_property_reading_be_t10, animal_status_kernel__property_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(animal_property_reading_be_t20, animal_status_kernel__property_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(animal_property_reading_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(animal_property_reading_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(animal_property_reading_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.87).
narrative_ontology:measurement(animal_property_reading_be_t60, animal_status_kernel__property_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(animal_property_reading_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(animal_property_reading_su_t10, animal_status_kernel__property_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(animal_property_reading_su_t20, animal_status_kernel__property_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(animal_property_reading_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(animal_property_reading_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(animal_property_reading_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement(animal_property_reading_su_t60, animal_status_kernel__property_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The moral status of animals is a colloquial label covering three structurally distinct claims; per the epsilon-invariance principle it is authored as a three-story family rather than one story with a measurement parameter. This file instantiates the property reading: animals as chattels, economic value exclusive, victim-set empty, epsilon 0.88. animal_status_kernel__welfare_reading instantiates the constrained-property claim: suffering morally relevant, partial victim-set, damped epsilon. animal_status_kernel__abolitionist_reading instantiates the personhood claim: property status itself the injury, maximal victim-set, maximal epsilon. All three assess the same standing arrangement from different seats. This reading is the legal baseline the other two contest: welfare law is administered through property doctrine and abolition litigation is dismissed under standing doctrine, so this file's network edges carry structural influence even though its axioms foreclose both siblings within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__property_reading, powerless, 0.97).
constraint_indexing:directionality_override(animal_status_kernel__property_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
